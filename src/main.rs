use std::time::{Duration, SystemTime};

use glutin_window::GlutinWindow as Window;
use opengl_graphics::{GlGraphics, OpenGL};
use piston::event_loop::{EventSettings, Events};
use piston::input::{RenderArgs, RenderEvent, UpdateArgs, UpdateEvent};
use piston::window::WindowSettings;
use piston::{Button, Key, PressEvent, ReleaseEvent};
use rand::rngs::ThreadRng;
use rand::Rng;

const WHITE: [f32; 4] = [1.0, 1.0, 1.0, 1.0];
const BLACK: [f32; 4] = [0.0, 0.0, 0.0, 1.0];

//const ON_COLOR_U8: [u8; 4] = [96, 96, 96, 255];
//const OFF_COLOR_U8: [u8; 4] = [15, 15, 15, 255];

const ON_COLOR_U8: [u8; 4] = [255, 170, 0, 255];
const OFF_COLOR_U8: [u8; 4] = [170, 68, 0, 255];

const ON_COLOR_F32: [f32; 4] = [
    ON_COLOR_U8[0] as f32 / 255.0,
    ON_COLOR_U8[1] as f32 / 255.0,
    ON_COLOR_U8[2] as f32 / 255.0,
    ON_COLOR_U8[3] as f32 / 255.0,
];
const OFF_COLOR_F32: [f32; 4] = [
    OFF_COLOR_U8[0] as f32 / 255.0,
    OFF_COLOR_U8[1] as f32 / 255.0,
    OFF_COLOR_U8[2] as f32 / 255.0,
    OFF_COLOR_U8[3] as f32 / 255.0,
];

const ON_COLOR: [f32; 4] = ON_COLOR_F32;
const OFF_COLOR: [f32; 4] = OFF_COLOR_F32;

const SCALE: u32 = 10;
const SCREEN_WIDTH: usize = 64;
const SCREEN_HEIGHT: usize = 32;
const MEM_SIZE: usize = 0x1000;
const LOAD_ADDR: usize = 0x0200;

const SHIFT_VY_TO_VX: bool = true;
const JUMP_OFFSET_BUG: bool = false;
const INCREMENT_I_ON_LOAD_STORE: bool = true;
const SPRITE_CLIPPING: bool = false;
const RESET_VF: bool = true;
const MAX_SPRITES_PER_FRAME: u32 = 1000;

const FONT_LOCATION: usize = 0;

fn main() {
    // Change this to OpenGL::V2_1 if not working.
    let opengl = OpenGL::V3_2;

    // Create a Glutin window.
    let mut window: Window = WindowSettings::new(
        "CHIP8 Emulator",
        [SCREEN_WIDTH as u32 * SCALE, SCREEN_HEIGHT as u32 * SCALE],
    )
    .graphics_api(opengl)
    .exit_on_esc(true)
    .build()
    .unwrap();

    let programs = [
        include_bytes!("octojam1title.ch8").to_vec(),
        include_bytes!("octojam2title.ch8").to_vec(),
        include_bytes!("octojam3title.ch8").to_vec(),
        include_bytes!("octojam4title.ch8").to_vec(),
        include_bytes!("octojam5title.ch8").to_vec(),
        include_bytes!("octojam6title.ch8").to_vec(),
        include_bytes!("octojam7title.ch8").to_vec(),
        include_bytes!("octojam8title.ch8").to_vec(),
        include_bytes!("octojam9title.ch8").to_vec(),
    ];

    let mut selected = 0;
    let mut last_program_start = SystemTime::now();

    let mut app = App::new(&programs[selected], opengl);

    let mut events = Events::new(EventSettings {
        max_fps: 60,
        ups: 1000,
        ups_reset: 0,
        swap_buffers: true,
        bench_mode: false,
        lazy: false,
    });

    while let Some(e) = events.next(&mut window) {
        if let Some(args) = e.render_args() {
            app.render(&args);
        }

        if let Some(args) = e.update_args() {
            app.tick(&args);
        }

        if let Some(Button::Keyboard(key)) = e.press_args() {
            if let Ok(key) = key.try_into() {
                app.key_press(key);
            }
        }

        if let Some(Button::Keyboard(key)) = e.release_args() {
            if let Ok(key) = key.try_into() {
                app.key_release(key);
            }
        }
        if SystemTime::now()
            .duration_since(last_program_start)
            .unwrap()
            > Duration::from_secs(10)
        {
            //load next program
            selected = (selected + 1) % programs.len();
            app = App::new(&programs[selected], opengl);
            last_program_start = SystemTime::now();
            println!("hi!");
        }
    }
}

pub struct App {
    gl: GlGraphics, // OpenGL drawing backend.
    screen: [[bool; SCREEN_HEIGHT]; SCREEN_WIDTH],
    memory: [u8; MEM_SIZE],
    program_counter: usize,
    stack: [usize; 48],
    stack_pointer: usize,
    registers: [u8; 16],
    register_i: u16,
    input: Input,
    timers: Timers,
    rng: ThreadRng,
    sprite_frame: u32,
}

impl App {
    fn new(program: &[u8], opengl: OpenGL) -> Self {
        let mut memory = [0; MEM_SIZE];
        let font = [
            0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
            0x20, 0x60, 0x20, 0x20, 0x70, // 1
            0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
            0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
            0x90, 0x90, 0xF0, 0x10, 0x10, // 4
            0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
            0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
            0xF0, 0x10, 0x20, 0x40, 0x40, // 7
            0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
            0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
            0xF0, 0x90, 0xF0, 0x90, 0x90, // A
            0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
            0xF0, 0x80, 0x80, 0x80, 0xF0, // C
            0xE0, 0x90, 0x90, 0x90, 0xE0, // D
            0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
            0xF0, 0x80, 0xF0, 0x80, 0x80, // F
        ];
        for i in 0..font.len() {
            memory[i + FONT_LOCATION] = font[i];
        }

        for i in 0..program.len() {
            memory[LOAD_ADDR + i] = program[i];
        }

        // Create a new game
        Self {
            gl: GlGraphics::new(opengl),
            memory,
            program_counter: LOAD_ADDR,
            screen: [[false; SCREEN_HEIGHT]; SCREEN_WIDTH],
            stack: [0; 48],
            stack_pointer: 0,
            registers: [0; 16],
            register_i: 0,
            input: Input::new(),
            timers: Timers::new(),
            rng: rand::thread_rng(),
            sprite_frame: 0,
        }
    }
    fn tick(&mut self, args: &UpdateArgs) {
        self.timers.tick(args.dt);
        /*if self.timers.get_delay_timer() != 0 {
            return;
        } else {
            self.timers.set_delay_timer(5);
        }*/
        let opcode = ((self.memory[self.program_counter] as u16) << 8)
            + self.memory[self.program_counter + 1] as u16;
        self.program_counter += 2;

        let beg = (opcode >> 12) as u8;
        let x = (opcode >> 8) % 16;
        let vx = self.registers[x as usize];
        let y = (opcode >> 4) % 16;
        let vy = self.registers[y as usize];
        let n = (opcode % 16) as u8;
        let nn = (opcode % (16 * 16)) as u8;
        let nnn = opcode % (16 * 16 * 16);
        /*println!(
            "{:#04x}: {:#04x}, {:#04x}, {:#04x}, {:#04x}, {:#04x}, {:#04x}",
            opcode, beg, x, y, n, nn, nnn
        );*/

        match beg {
            0x0 => {
                match opcode {
                    0x00E0 => {
                        //clear screen
                        self.screen = [[false; SCREEN_HEIGHT]; SCREEN_WIDTH];
                        /*self.gl.draw(args.viewport(), |c, gl| {
                            graphics::clear(OFF_COLOR, gl);
                        });*/
                    }
                    0x00EE => {
                        //return from call (2NNN)
                        //stack pointer points 1 past the last element
                        self.program_counter = self.stack[self.stack_pointer - 1];
                        //decrement stack pointer
                        self.stack_pointer -= 1;
                    }
                    _ => unimplemented!(),
                }
            }
            0x1 => {
                //jump
                self.program_counter = nnn as usize;
            }

            0x2 => {
                //subroutine jump (jump with ability to jump back (function))
                //set last element of stack to current program counter
                self.stack[self.stack_pointer] = self.program_counter;
                self.stack_pointer += 1; //increment stack pointer

                self.program_counter = nnn as usize; //jump to new location
            }

            0x3 => {
                //skip next instruction if vx == nn literal
                if vx == nn {
                    self.program_counter += 2;
                }
            }

            0x4 => {
                //skip next instruction if vx != nn literal
                if vx != nn {
                    self.program_counter += 2;
                }
            }

            0x5 => {
                //skips next instruction if vx == vy
                if vx == vy {
                    self.program_counter += 2;
                }
            }

            0x6 => {
                //set register VX to nn literal
                self.registers[x as usize] = nn;
            }

            0x7 => {
                //add nn literal to VX
                //don't change flag
                //convert to u16 and back to avoid overflow errors
                self.registers[x as usize] = ((vx as u16 + nn as u16) % 0x0100) as u8;
            }

            0x8 => {
                //logical/arithmetic instructions
                //0x8XYN
                match n {
                    0x0 => {
                        //set VX to VY
                        self.registers[x as usize] = vy;
                    }
                    0x1 => {
                        //bitwise OR of VX and VY
                        self.registers[x as usize] |= vy;

                        if RESET_VF {
                            self.registers[0xf] = 0;
                        }
                    }
                    0x2 => {
                        //bitwise AND of VX and VY
                        self.registers[x as usize] &= vy;

                        if RESET_VF {
                            self.registers[0xf] = 0;
                        }
                    }
                    0x3 => {
                        //bitwise xor of VX and VY
                        self.registers[x as usize] ^= vy;

                        if RESET_VF {
                            self.registers[0xf] = 0;
                        }
                    }
                    0x4 => {
                        //add VX and VY to VX
                        //set VF to 1 if VX + VY > 255, otherwise 0
                        let uncapped_add = vx as u16 + vy as u16;

                        //set VX to result
                        self.registers[x as usize] = (uncapped_add % 0x0100) as u8;

                        //set VF (carry flag)
                        self.registers[0xF] = if uncapped_add >= 0x0100 { 1 } else { 0 };
                    }
                    0x5 => {
                        //Subtract, VX = VX - VY
                        //set VF to 0 if underflow (VX < VY) and 1 otherwise
                        let mut vx = vx as u16;
                        let underflow = if vx < vy as u16 {
                            //if vx is too small, add carry bit at position 9
                            vx = vx | 0b1_0000_0000;
                            //set undeflow bit
                            0
                        } else {
                            //unset underflow bit
                            1
                        };

                        //set vx to result
                        self.registers[x as usize] = (vx - vy as u16) as u8;
                        self.registers[0xf] = underflow;
                    }
                    0x6 => {
                        //shift right, configurably copy VY ro VX, set vf to shifted bit
                        let mut vx = vx;
                        if SHIFT_VY_TO_VX {
                            self.registers[x as usize] = vy;
                            vx = vy;
                        }
                        self.registers[x as usize] >>= 1;
                        self.registers[0xF] = vx % 2;
                    }
                    0x7 => {
                        //Subtract, VX = VY - VX
                        //set VF to 0 if underflox (VY < VX) and 1 otherwise
                        let mut vy = vy as u16;
                        let underflow = if vy < vx as u16 {
                            //if vy is too small, add carry bit at position 9
                            vy = vy | 0b1_0000_0000;
                            //set undeflow bit
                            0
                        } else {
                            //unset underflow bit
                            1
                        };

                        //set vx to result
                        self.registers[x as usize] = (vy - vx as u16) as u8;
                        self.registers[0xf] = underflow;
                    }
                    0xe => {
                        //shift left, configurably copy VY ro VX, set vf to shifted bit

                        let mut vx = vx;
                        if SHIFT_VY_TO_VX {
                            self.registers[x as usize] = vy;
                            vx = vy;
                        }
                        self.registers[x as usize] <<= 1;
                        self.registers[0xF] = vx >> 7;
                    }
                    _ => unimplemented!(),
                }
            }

            0x9 => {
                //skip next instruction if vx != vy
                if vx != vy {
                    self.program_counter += 2;
                }
            }

            0xA => {
                //set index register I to nnn literal
                self.register_i = nnn;
            }

            0xB => {
                //jump offset
                //jumps to NNN + V0 OR NNN + VX depending on config
                if JUMP_OFFSET_BUG {
                    //NNN + VX
                    self.program_counter = nnn as usize + vx as usize;
                } else {
                    //NNN + V0
                    self.program_counter = nnn as usize + self.registers[0x0] as usize;
                }
            }
            0xC => {
                //set vx to a random number anded with nn (vx = rand & nn)
                self.registers[x as usize] = self.rng.gen::<u8>() & nn;
            }

            0xD => {
                //draw

                //limit sprites/frame
                if self.sprite_frame < MAX_SPRITES_PER_FRAME {
                    self.sprite_frame += 1;
                } else {
                    //loop back to this instruction
                    self.program_counter -= 2;
                    //early return
                    return;
                }
                //set VF register to 0 (no pixels flipped from on to off)
                self.registers[0xf] = 0;

                //wrap vx and vy around (so none of it is off screen)
                let vx = vx % SCREEN_WIDTH as u8;
                let vy = vy % SCREEN_HEIGHT as u8;

                for y_off in 0..n as usize {
                    let sprite_byte = self.memory[self.register_i as usize + y_off];
                    for x_off in 0..8_usize {
                        let mut x_pos = vx as usize + x_off;
                        let mut y_pos = vy as usize + y_off;

                        if !SPRITE_CLIPPING {
                            x_pos %= SCREEN_WIDTH;
                            y_pos %= SCREEN_HEIGHT;
                        }

                        if x_pos < SCREEN_WIDTH && y_pos < SCREEN_HEIGHT {
                            let sprite_bit = ((sprite_byte >> (7 - x_off)) % 2) == 1;

                            let pixel_state = self.screen[x_pos][y_pos];

                            let new_state = pixel_state ^ sprite_bit;

                            if pixel_state == true && new_state == false {
                                self.registers[0xf] = 1;
                            }

                            self.screen[x_pos][y_pos] = new_state;

                            /*if pixel_state != new_state {
                                let color = if new_state { ON_COLOR } else { OFF_COLOR };
                                self.gl.draw(args.viewport(), |c, gl| {
                                    graphics::rectangle(
                                        color,
                                        graphics::rectangle::square(
                                            (x_pos as u32 * SCALE) as f64,
                                            (y_pos as u32 * SCALE) as f64,
                                            SCALE as f64,
                                        ),
                                        c.transform,
                                        gl,
                                    );
                                });
                            }*/
                        }
                    }
                }
            }

            0xE => {
                //input key operations
                //format: EXNN
                match nn {
                    0x9E => {
                        //skip next instruction if key VX is pressed
                        if self.input.is_pressed_num(vx) {
                            self.program_counter += 2;
                        }
                    }
                    0xA1 => {
                        //skip next instruction if key VX is not pressed
                        if !self.input.is_pressed_num(vx) {
                            self.program_counter += 2;
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            0xF => {
                //misc instructions
                //format: FXnn
                match nn {
                    0x07 => {
                        //set VX to delay timer value
                        self.registers[x as usize] = self.timers.get_delay_timer();
                    }
                    0x15 => {
                        //set Delay timer to VX
                        self.timers.set_delay_timer(vx);
                    }
                    0x18 => {
                        //set sound timer to vx
                        self.timers.set_sound_timer(vx);
                    }

                    0x1E => {
                        //add VX to I

                        //non-default behavior: VF overflow bit is used
                        //I is 12 bits wide, cap it to that

                        self.register_i += vx as u16;
                        //overflow
                        if self.register_i >= 0x1000 {
                            self.register_i %= 0x1000;
                            self.registers[0xf] = 1;
                        } else {
                            self.registers[0xf] = 0;
                        }
                    }

                    0x0A => {
                        //get key
                        //wait until key is pressed and save it to vx

                        if let Some(key) = self.input.wait_for_key() {
                            //set vx
                            self.registers[x as usize] = key;
                        } else {
                            //if we don't have a key yet, decrement program counter back to this instruction
                            //(loop back)
                            self.program_counter -= 2;
                        }
                    }

                    0x29 => {
                        //set I to the location of the char corresponding to VX
                        //only take the first nibble of VX
                        self.register_i = FONT_LOCATION as u16 + (vx % 0x10) as u16 * 5;
                    }

                    0x33 => {
                        //write VX as BCD 255 (FF) => 2, 5, 5 in i, i+1, i+2
                        let mut vx = vx;
                        let reg_i = self.register_i as usize;
                        self.memory[reg_i + 2] = vx % 10;
                        vx /= 10;
                        self.memory[reg_i + 1] = vx % 10;
                        vx /= 10;
                        self.memory[reg_i] = vx % 10;
                    }

                    0x55 => {
                        //Store V0..VX one after another starting at I
                        for offset in 0..=x as usize {
                            self.memory[self.register_i as usize + offset] = self.registers[offset];
                        }
                        //for really old games where I was incremented as it was saving the registers
                        if INCREMENT_I_ON_LOAD_STORE {
                            self.register_i += x as u16 + 1;
                        }
                    }
                    0x65 => {
                        //loads V0..VX one after another starting at I
                        for offset in 0..=x as usize {
                            self.registers[offset] = self.memory[self.register_i as usize + offset];
                        }
                        //for really old games where I was incremented as it was loading the registers
                        if INCREMENT_I_ON_LOAD_STORE {
                            self.register_i += x as u16 + 1;
                        }
                    }
                    _ => unimplemented!(),
                }
            }

            (0x10..=0xFF) => unreachable!(),
        }
    }

    fn render(&mut self, args: &RenderArgs) {
        //only render if sprites were drawn
        if self.sprite_frame > 0 {
            //reset sprite drawn count
            self.sprite_frame = 0;

            self.gl.draw(args.viewport(), |c, gl| {
                graphics::clear(OFF_COLOR, gl);

                for x in 0..SCREEN_WIDTH as u32 {
                    for y in 0..SCREEN_HEIGHT as u32 {
                        if self.screen[x as usize][y as usize] {
                            graphics::rectangle(
                                ON_COLOR,
                                graphics::rectangle::square(
                                    (x * SCALE) as f64,
                                    (y * SCALE) as f64,
                                    SCALE as f64,
                                ),
                                c.transform,
                                gl,
                            )
                        }
                    }
                }
            });
        }
    }

    fn key_press(&mut self, key: KeyInput) {
        self.input.press(key);
    }

    fn key_release(&mut self, key: KeyInput) {
        self.input.release(key);
    }
}

enum WaitingForKey {
    NotWaiting,
    Waiting,
    Pressed(Vec<u8>),
    Released(u8),
}
struct Input {
    keys: [bool; 16],
    wait_key: WaitingForKey,
}
impl Input {
    pub fn new() -> Self {
        Self {
            keys: [false; 16],
            wait_key: WaitingForKey::NotWaiting,
        }
    }

    pub fn press(&mut self, key: KeyInput) {
        let key = key as u8;
        self.keys[key as usize] = true;
        if let WaitingForKey::Waiting = self.wait_key {
            self.wait_key = WaitingForKey::Pressed(vec![key]);
        } else if let WaitingForKey::Pressed(keys) = &mut self.wait_key {
            keys.push(key);
        }
    }
    pub fn release(&mut self, key: KeyInput) {
        let key = key as u8;
        self.keys[key as usize] = false;
        if let WaitingForKey::Pressed(keys) = &mut self.wait_key {
            if keys.contains(&key) {
                self.wait_key = WaitingForKey::Released(key);
            }
        }
    }

    pub fn is_pressed(&self, key: KeyInput) -> bool {
        self.keys[key as usize]
    }
    pub fn is_pressed_num(&self, key: u8) -> bool {
        self.keys[key as usize]
    }

    pub fn wait_for_key(&mut self) -> Option<u8> {
        match self.wait_key {
            WaitingForKey::NotWaiting => {
                self.wait_key = WaitingForKey::Waiting;
                None
            }
            WaitingForKey::Waiting | WaitingForKey::Pressed(_) => None,
            WaitingForKey::Released(key) => {
                self.wait_key = WaitingForKey::NotWaiting;
                Some(key)
            }
        }
    }
}
enum KeyInput {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
    I,
    J,
    K,
    L,
    M,
    N,
    O,
    P,
}
impl TryFrom<Key> for KeyInput {
    type Error = ();

    fn try_from(value: Key) -> Result<Self, Self::Error> {
        Ok(match value {
            Key::D1 => Self::B,
            Key::D2 => Self::C,
            Key::D3 => Self::D,
            Key::D4 => Self::M,

            Key::Q => Self::E,
            Key::W => Self::F,
            Key::E => Self::G,
            Key::R => Self::N,

            Key::A => Self::H,
            Key::S => Self::I,
            Key::D => Self::J,
            Key::F => Self::O,

            Key::Z => Self::K,
            Key::X => Self::A,
            Key::C => Self::L,
            Key::V => Self::P,
            _ => return Err(()),
        })
    }
}

struct Timers {
    delay_timer: Option<f64>,
    sound_timer: Option<f64>,
    last_delay: u8,
    last_sound: u8,
}

impl Timers {
    pub fn new() -> Self {
        Self {
            delay_timer: None,
            sound_timer: None,
            last_delay: 0,
            last_sound: 0,
        }
    }

    pub fn tick(&mut self, dt: f64) {
        if let Some(time) = &mut self.delay_timer {
            *time -= dt * 60.0;
            println!("Delay time: {time}");
            if *time <= 0.0 {
                self.delay_timer = None;
                self.last_delay = 0;
            } else {
                self.last_delay = time.floor() as u8;
            }
        }

        if let Some(time) = &mut self.sound_timer {
            *time -= dt;
            if *time <= 0.0 {
                self.sound_timer = None;
                self.last_sound = 0;
            } else {
                self.last_sound = time.floor() as u8;
            }
        }
    }

    pub fn set_sound_timer(&mut self, time: u8) {
        self.sound_timer = Some(time as f64);
    }

    pub fn set_delay_timer(&mut self, time: u8) {
        self.delay_timer = Some(time as f64);
    }
    pub fn get_delay_timer(&mut self) -> u8 {
        self.last_delay
    }
}
