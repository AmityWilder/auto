use crate::screen::Hdc;
use enigo::{
    Button, Coordinate,
    Direction::{Click, Press, Release},
    Enigo, Key, Keyboard, Mouse, Settings,
};

mod screen;

fn main() {
    let mut input = Enigo::new(&Settings::default()).unwrap();
    input.move_mouse(50, 50, Coordinate::Rel).unwrap();
    input.button(Button::Left, Click).unwrap();
    let (mouse_x, mouse_y) = input.location().unwrap();

    let mut hdc = unsafe { Hdc::new(None) }.unwrap();
    let px = unsafe { hdc.get_pixel(mouse_x, mouse_y) }.unwrap().to_rgb();
    println!("Color at ({mouse_x}, {mouse_y}): {px:?}");
}
