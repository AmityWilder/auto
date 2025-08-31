pub use enigo::{Button, Coordinate, Direction, InputError, Key, Keyboard, Mouse};
use enigo::{Enigo, InputResult, Settings};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InputSettings {
    #[cfg(target_os = "linux")]
    linux_delay: u32,
    #[cfg(target_os = "linux")]
    x11_display: Option<String>,
    #[cfg(target_os = "linux")]
    wayland_display: Option<String>,
    #[cfg(target_os = "windows")]
    windows_dw_extra_info: Option<usize>,
    event_source_user_data: Option<i64>,
    release_keys_when_dropped: bool,
    #[cfg(target_os = "macos")]
    open_prompt_to_get_permissions: bool,
    #[cfg(target_os = "macos")]
    independent_of_keyboard_state: bool,
    #[cfg(target_os = "windows")]
    windows_subject_to_mouse_speed_and_acceleration_level: bool,
}

impl Default for InputSettings {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl InputSettings {
    pub const fn new() -> Self {
        Self {
            #[cfg(target_os = "linux")]
            linux_delay: 12,
            #[cfg(target_os = "linux")]
            x11_display: None,
            #[cfg(target_os = "linux")]
            wayland_display: None,
            #[cfg(target_os = "windows")]
            windows_dw_extra_info: None,
            event_source_user_data: None,
            release_keys_when_dropped: true,
            #[cfg(target_os = "macos")]
            open_prompt_to_get_permissions: true,
            #[cfg(target_os = "macos")]
            independent_of_keyboard_state: true,
            #[cfg(target_os = "windows")]
            windows_subject_to_mouse_speed_and_acceleration_level: false,
        }
    }

    #[cfg(target_os = "linux")]
    /// Sleep delay on Linux X11
    pub fn linux_delay(&mut self, value: u32) -> &mut Self {
        self.linux_delay = value;
        self
    }

    #[cfg(target_os = "linux")]
    /// Display name to connect to when using Linux X11
    pub fn x11_display(&mut self, value: Option<String>) -> &mut Self {
        self.x11_display = value;
        self
    }

    #[cfg(target_os = "linux")]
    /// Display name to connect to when using Linux Wayland
    pub fn wayland_display(&mut self, value: Option<String>) -> &mut Self {
        self.wayland_display = value;
        self
    }

    #[cfg(target_os = "windows")]
    /// Arbitrary value to be able to distinguish events created by enigo
    /// All events will be marked with this value in the dwExtraInfo field
    pub fn windows_dw_extra_info(&mut self, value: Option<usize>) -> &mut Self {
        self.windows_dw_extra_info = value;
        self
    }

    /// Arbitrary value to be able to distinguish events created by enigo
    /// All events will be marked with this value in the
    /// `EVENT_SOURCE_USER_DATA` field
    pub fn event_source_user_data(&mut self, value: Option<i64>) -> &mut Self {
        self.event_source_user_data = value;
        self
    }

    /// Set this to true if you want all held keys to get released when Enigo
    /// gets dropped. The default is true.
    pub fn release_keys_when_dropped(&mut self, value: bool) -> &mut Self {
        self.release_keys_when_dropped = value;
        self
    }

    #[cfg(target_os = "macos")]
    /// Open a prompt to ask the user for the permission to simulate input if
    /// they are missing. This only works on macOS. The default is true.
    pub fn open_prompt_to_get_permissions(&mut self, value: bool) -> &mut Self {
        self.open_prompt_to_get_permissions = value;
        self
    }

    #[cfg(target_os = "macos")]
    /// The simulated input is independent from the pressed keys on the
    /// physical keyboard. This only works on macOS.
    /// The default is true. If the Shift key for example is pressed,
    /// following simulated input will not be capitalized.
    pub fn independent_of_keyboard_state(&mut self, value: bool) -> &mut Self {
        self.independent_of_keyboard_state = value;
        self
    }

    #[cfg(target_os = "windows")]
    /// If this is set to true, the relative mouse motion will be subject to the
    /// settings for mouse speed and acceleration level. An end user sets
    /// these values using the Mouse application in Control Panel. An
    /// application obtains and sets these values with the
    /// [`windows::Win32::UI::WindowsAndMessaging::SystemParametersInfoA`]
    /// function. The default value is false.
    pub fn windows_subject_to_mouse_speed_and_acceleration_level(
        &mut self,
        value: bool,
    ) -> &mut Self {
        self.windows_subject_to_mouse_speed_and_acceleration_level = value;
        self
    }
}

impl From<InputSettings> for Settings {
    #[inline]
    fn from(value: InputSettings) -> Self {
        let InputSettings {
            #[cfg(target_os = "linux")]
            linux_delay,
            #[cfg(target_os = "linux")]
            x11_display,
            #[cfg(target_os = "linux")]
            wayland_display,
            #[cfg(target_os = "windows")]
            windows_dw_extra_info,
            event_source_user_data,
            release_keys_when_dropped,
            #[cfg(target_os = "macos")]
            open_prompt_to_get_permissions,
            #[cfg(target_os = "macos")]
            independent_of_keyboard_state,
            #[cfg(target_os = "windows")]
            windows_subject_to_mouse_speed_and_acceleration_level,
        } = value;

        Settings {
            #[cfg(target_os = "linux")]
            linux_delay,
            #[cfg(target_os = "linux")]
            x11_display,
            #[cfg(target_os = "linux")]
            wayland_display,
            #[cfg(target_os = "windows")]
            windows_dw_extra_info,
            event_source_user_data,
            release_keys_when_dropped,
            #[cfg(target_os = "macos")]
            open_prompt_to_get_permissions,
            #[cfg(target_os = "macos")]
            independent_of_keyboard_state,
            #[cfg(target_os = "windows")]
            windows_subject_to_mouse_speed_and_acceleration_level,
            ..Settings::default()
        }
    }
}

#[derive(Debug)]
pub struct Input(Enigo);

impl std::ops::Deref for Input {
    type Target = Enigo;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for Input {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Input {
    #[inline]
    pub fn new() -> Result<Self, enigo::NewConError> {
        Self::with_settings(InputSettings::new())
    }

    #[inline]
    pub fn with_settings(settings: InputSettings) -> Result<Self, enigo::NewConError> {
        Enigo::new(&settings.into()).map(Self)
    }

    #[inline]
    pub fn move_mouse_rel(&mut self, x: i32, y: i32) -> InputResult<()> {
        self.move_mouse(x, y, Coordinate::Rel)
    }

    #[inline]
    pub fn move_mouse_abs(&mut self, x: i32, y: i32) -> InputResult<()> {
        self.move_mouse(x, y, Coordinate::Abs)
    }

    #[inline]
    pub fn move_mouse(&mut self, x: i32, y: i32, coordinate: Coordinate) -> InputResult<()> {
        self.0.move_mouse(x, y, coordinate)
    }

    #[inline]
    pub fn key_press(&mut self, key: Key) -> InputResult<()> {
        self.key(key, Direction::Press)
    }

    #[inline]
    pub fn key_release(&mut self, key: Key) -> InputResult<()> {
        self.key(key, Direction::Release)
    }

    #[inline]
    pub fn key_click(&mut self, key: Key) -> InputResult<()> {
        self.key(key, Direction::Click)
    }

    #[inline]
    pub fn key(&mut self, key: Key, direction: Direction) -> InputResult<()> {
        self.0.key(key, direction)
    }

    #[inline]
    pub fn button_press(&mut self, button: Button) -> InputResult<()> {
        self.button(button, Direction::Press)
    }

    #[inline]
    pub fn button_release(&mut self, button: Button) -> InputResult<()> {
        self.button(button, Direction::Release)
    }

    #[inline]
    pub fn button_click(&mut self, button: Button) -> InputResult<()> {
        self.button(button, Direction::Click)
    }

    #[inline]
    pub fn button(&mut self, button: Button, direction: Direction) -> InputResult<()> {
        self.0.button(button, direction)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct KeyboardKey(pub Key);

impl KeyboardKey {
    #[inline]
    pub const fn to_ne_bytes(self) -> [u8; std::mem::size_of::<Key>()] {
        unsafe { std::mem::transmute(self) }
    }

    #[inline]
    pub const fn from_ne_bytes(value: [u8; std::mem::size_of::<Key>()]) -> Self {
        unsafe { std::mem::transmute(value) }
    }
}

impl From<Key> for KeyboardKey {
    #[inline]
    fn from(value: Key) -> Self {
        Self(value)
    }
}

impl From<KeyboardKey> for Key {
    #[inline]
    fn from(value: KeyboardKey) -> Self {
        value.0
    }
}

impl std::str::FromStr for KeyboardKey {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            #[cfg(target_os = "windows")]
            "Num0" => Ok(Self(Key::Num0)),
            #[cfg(target_os = "windows")]
            "Num1" => Ok(Self(Key::Num1)),
            #[cfg(target_os = "windows")]
            "Num2" => Ok(Self(Key::Num2)),
            #[cfg(target_os = "windows")]
            "Num3" => Ok(Self(Key::Num3)),
            #[cfg(target_os = "windows")]
            "Num4" => Ok(Self(Key::Num4)),
            #[cfg(target_os = "windows")]
            "Num5" => Ok(Self(Key::Num5)),
            #[cfg(target_os = "windows")]
            "Num6" => Ok(Self(Key::Num6)),
            #[cfg(target_os = "windows")]
            "Num7" => Ok(Self(Key::Num7)),
            #[cfg(target_os = "windows")]
            "Num8" => Ok(Self(Key::Num8)),
            #[cfg(target_os = "windows")]
            "Num9" => Ok(Self(Key::Num9)),
            #[cfg(target_os = "windows")]
            "A" => Ok(Self(Key::A)),
            #[cfg(target_os = "windows")]
            "B" => Ok(Self(Key::B)),
            #[cfg(target_os = "windows")]
            "C" => Ok(Self(Key::C)),
            #[cfg(target_os = "windows")]
            "D" => Ok(Self(Key::D)),
            #[cfg(target_os = "windows")]
            "E" => Ok(Self(Key::E)),
            #[cfg(target_os = "windows")]
            "F" => Ok(Self(Key::F)),
            #[cfg(target_os = "windows")]
            "G" => Ok(Self(Key::G)),
            #[cfg(target_os = "windows")]
            "H" => Ok(Self(Key::H)),
            #[cfg(target_os = "windows")]
            "I" => Ok(Self(Key::I)),
            #[cfg(target_os = "windows")]
            "J" => Ok(Self(Key::J)),
            #[cfg(target_os = "windows")]
            "K" => Ok(Self(Key::K)),
            #[cfg(target_os = "windows")]
            "L" => Ok(Self(Key::L)),
            #[cfg(target_os = "windows")]
            "M" => Ok(Self(Key::M)),
            #[cfg(target_os = "windows")]
            "N" => Ok(Self(Key::N)),
            #[cfg(target_os = "windows")]
            "O" => Ok(Self(Key::O)),
            #[cfg(target_os = "windows")]
            "P" => Ok(Self(Key::P)),
            #[cfg(target_os = "windows")]
            "Q" => Ok(Self(Key::Q)),
            #[cfg(target_os = "windows")]
            "R" => Ok(Self(Key::R)),
            #[cfg(target_os = "windows")]
            "S" => Ok(Self(Key::S)),
            #[cfg(target_os = "windows")]
            "T" => Ok(Self(Key::T)),
            #[cfg(target_os = "windows")]
            "U" => Ok(Self(Key::U)),
            #[cfg(target_os = "windows")]
            "V" => Ok(Self(Key::V)),
            #[cfg(target_os = "windows")]
            "W" => Ok(Self(Key::W)),
            #[cfg(target_os = "windows")]
            "X" => Ok(Self(Key::X)),
            #[cfg(target_os = "windows")]
            "Y" => Ok(Self(Key::Y)),
            #[cfg(target_os = "windows")]
            "Z" => Ok(Self(Key::Z)),
            #[cfg(target_os = "windows")]
            "AbntC1" => Ok(Self(Key::AbntC1)),
            #[cfg(target_os = "windows")]
            "AbntC2" => Ok(Self(Key::AbntC2)),
            #[cfg(target_os = "windows")]
            "Accept" => Ok(Self(Key::Accept)),
            "Add" => Ok(Self(Key::Add)),
            "Alt" => Ok(Self(Key::Alt)),
            #[cfg(target_os = "windows")]
            "Apps" => Ok(Self(Key::Apps)),
            #[cfg(target_os = "windows")]
            "Attn" => Ok(Self(Key::Attn)),
            "Backspace" => Ok(Self(Key::Backspace)),
            #[cfg(all(unix, not(target_os = "macos")))]
            "Break" => Ok(Self(Key::Break)),
            #[cfg(all(unix, not(target_os = "macos")))]
            "Begin" => Ok(Self(Key::Begin)),
            #[cfg(target_os = "macos")]
            "BrightnessDown" => Ok(Self(Key::BrightnessDown)),
            #[cfg(target_os = "macos")]
            "BrightnessUp" => Ok(Self(Key::BrightnessUp)),
            #[cfg(target_os = "windows")]
            "BrowserBack" => Ok(Self(Key::BrowserBack)),
            #[cfg(target_os = "windows")]
            "BrowserFavorites" => Ok(Self(Key::BrowserFavorites)),
            #[cfg(target_os = "windows")]
            "BrowserForward" => Ok(Self(Key::BrowserForward)),
            #[cfg(target_os = "windows")]
            "BrowserHome" => Ok(Self(Key::BrowserHome)),
            #[cfg(target_os = "windows")]
            "BrowserRefresh" => Ok(Self(Key::BrowserRefresh)),
            #[cfg(target_os = "windows")]
            "BrowserSearch" => Ok(Self(Key::BrowserSearch)),
            #[cfg(target_os = "windows")]
            "BrowserStop" => Ok(Self(Key::BrowserStop)),
            #[cfg(any(target_os = "windows", all(unix, not(target_os = "macos"))))]
            "Cancel" => Ok(Self(Key::Cancel)),
            "CapsLock" => Ok(Self(Key::CapsLock)),
            #[cfg(any(target_os = "windows", all(unix, not(target_os = "macos"))))]
            "Clear" => Ok(Self(Key::Clear)),
            #[cfg(target_os = "macos")]
            "ContrastUp" => Ok(Self(Key::ContrastUp)),
            #[cfg(target_os = "macos")]
            "ContrastDown" => Ok(Self(Key::ContrastDown)),
            "Control" | "Ctrl" => Ok(Self(Key::Control)),
            #[cfg(target_os = "windows")]
            "Convert" => Ok(Self(Key::Convert)),
            #[cfg(target_os = "windows")]
            "Crsel" => Ok(Self(Key::Crsel)),
            #[cfg(target_os = "windows")]
            "DBEAlphanumeric" => Ok(Self(Key::DBEAlphanumeric)),
            #[cfg(target_os = "windows")]
            "DBECodeinput" => Ok(Self(Key::DBECodeinput)),
            #[cfg(target_os = "windows")]
            "DBEDetermineString" => Ok(Self(Key::DBEDetermineString)),
            #[cfg(target_os = "windows")]
            "DBEEnterDLGConversionMode" => Ok(Self(Key::DBEEnterDLGConversionMode)),
            #[cfg(target_os = "windows")]
            "DBEEnterIMEConfigMode" => Ok(Self(Key::DBEEnterIMEConfigMode)),
            #[cfg(target_os = "windows")]
            "DBEEnterWordRegisterMode" => Ok(Self(Key::DBEEnterWordRegisterMode)),
            #[cfg(target_os = "windows")]
            "DBEFlushString" => Ok(Self(Key::DBEFlushString)),
            #[cfg(target_os = "windows")]
            "DBEHiragana" => Ok(Self(Key::DBEHiragana)),
            #[cfg(target_os = "windows")]
            "DBEKatakana" => Ok(Self(Key::DBEKatakana)),
            #[cfg(target_os = "windows")]
            "DBENoCodepoint" => Ok(Self(Key::DBENoCodepoint)),
            #[cfg(target_os = "windows")]
            "DBENoRoman" => Ok(Self(Key::DBENoRoman)),
            #[cfg(target_os = "windows")]
            "DBERoman" => Ok(Self(Key::DBERoman)),
            #[cfg(target_os = "windows")]
            "DBESBCSChar" => Ok(Self(Key::DBESBCSChar)),
            #[cfg(target_os = "windows")]
            "DBESChar" => Ok(Self(Key::DBESChar)),
            "Decimal" => Ok(Self(Key::Decimal)),
            "Delete" => Ok(Self(Key::Delete)),
            "Divide" => Ok(Self(Key::Divide)),
            "DownArrow" => Ok(Self(Key::DownArrow)),
            #[cfg(target_os = "macos")]
            "Eject" => Ok(Self(Key::Eject)),
            "End" => Ok(Self(Key::End)),
            #[cfg(target_os = "windows")]
            "Ereof" => Ok(Self(Key::Ereof)),
            "Escape" => Ok(Self(Key::Escape)),
            #[cfg(any(target_os = "windows", all(unix, not(target_os = "macos"))))]
            "Execute" => Ok(Self(Key::Execute)),
            #[cfg(target_os = "windows")]
            "Exsel" => Ok(Self(Key::Exsel)),
            "F1" => Ok(Self(Key::F1)),
            "F2" => Ok(Self(Key::F2)),
            "F3" => Ok(Self(Key::F3)),
            "F4" => Ok(Self(Key::F4)),
            "F5" => Ok(Self(Key::F5)),
            "F6" => Ok(Self(Key::F6)),
            "F7" => Ok(Self(Key::F7)),
            "F8" => Ok(Self(Key::F8)),
            "F9" => Ok(Self(Key::F9)),
            "F10" => Ok(Self(Key::F10)),
            "F11" => Ok(Self(Key::F11)),
            "F12" => Ok(Self(Key::F12)),
            "F13" => Ok(Self(Key::F13)),
            "F14" => Ok(Self(Key::F14)),
            "F15" => Ok(Self(Key::F15)),
            "F16" => Ok(Self(Key::F16)),
            "F17" => Ok(Self(Key::F17)),
            "F18" => Ok(Self(Key::F18)),
            "F19" => Ok(Self(Key::F19)),
            "F20" => Ok(Self(Key::F20)),
            #[cfg(any(target_os = "windows", all(unix, not(target_os = "macos"))))]
            "F21" => Ok(Self(Key::F21)),
            #[cfg(any(target_os = "windows", all(unix, not(target_os = "macos"))))]
            "F22" => Ok(Self(Key::F22)),
            #[cfg(any(target_os = "windows", all(unix, not(target_os = "macos"))))]
            "F23" => Ok(Self(Key::F23)),
            #[cfg(any(target_os = "windows", all(unix, not(target_os = "macos"))))]
            "F24" => Ok(Self(Key::F24)),
            #[cfg(all(unix, not(target_os = "macos")))]
            "F25" => Ok(Self(Key::F25)),
            #[cfg(all(unix, not(target_os = "macos")))]
            "F26" => Ok(Self(Key::F26)),
            #[cfg(all(unix, not(target_os = "macos")))]
            "F27" => Ok(Self(Key::F27)),
            #[cfg(all(unix, not(target_os = "macos")))]
            "F28" => Ok(Self(Key::F28)),
            #[cfg(all(unix, not(target_os = "macos")))]
            "F29" => Ok(Self(Key::F29)),
            #[cfg(all(unix, not(target_os = "macos")))]
            "F30" => Ok(Self(Key::F30)),
            #[cfg(all(unix, not(target_os = "macos")))]
            "F31" => Ok(Self(Key::F31)),
            #[cfg(all(unix, not(target_os = "macos")))]
            "F32" => Ok(Self(Key::F32)),
            #[cfg(all(unix, not(target_os = "macos")))]
            "F33" => Ok(Self(Key::F33)),
            #[cfg(all(unix, not(target_os = "macos")))]
            "F34" => Ok(Self(Key::F34)),
            #[cfg(all(unix, not(target_os = "macos")))]
            "F35" => Ok(Self(Key::F35)),
            #[cfg(target_os = "macos")]
            "Function" => Ok(Self(Key::Function)),
            #[cfg(target_os = "windows")]
            "Final" => Ok(Self(Key::Final)),
            #[cfg(all(unix, not(target_os = "macos")))]
            "Find" => Ok(Self(Key::Find)),
            #[cfg(target_os = "windows")]
            "GamepadA" => Ok(Self(Key::GamepadA)),
            #[cfg(target_os = "windows")]
            "GamepadB" => Ok(Self(Key::GamepadB)),
            #[cfg(target_os = "windows")]
            "GamepadDPadDown" => Ok(Self(Key::GamepadDPadDown)),
            #[cfg(target_os = "windows")]
            "GamepadDPadLeft" => Ok(Self(Key::GamepadDPadLeft)),
            #[cfg(target_os = "windows")]
            "GamepadDPadRight" => Ok(Self(Key::GamepadDPadRight)),
            #[cfg(target_os = "windows")]
            "GamepadDPadUp" => Ok(Self(Key::GamepadDPadUp)),
            #[cfg(target_os = "windows")]
            "GamepadLeftShoulder" => Ok(Self(Key::GamepadLeftShoulder)),
            #[cfg(target_os = "windows")]
            "GamepadLeftThumbstickButton" => Ok(Self(Key::GamepadLeftThumbstickButton)),
            #[cfg(target_os = "windows")]
            "GamepadLeftThumbstickDown" => Ok(Self(Key::GamepadLeftThumbstickDown)),
            #[cfg(target_os = "windows")]
            "GamepadLeftThumbstickLeft" => Ok(Self(Key::GamepadLeftThumbstickLeft)),
            #[cfg(target_os = "windows")]
            "GamepadLeftThumbstickRight" => Ok(Self(Key::GamepadLeftThumbstickRight)),
            #[cfg(target_os = "windows")]
            "GamepadLeftThumbstickUp" => Ok(Self(Key::GamepadLeftThumbstickUp)),
            #[cfg(target_os = "windows")]
            "GamepadLeftTrigger" => Ok(Self(Key::GamepadLeftTrigger)),
            #[cfg(target_os = "windows")]
            "GamepadMenu" => Ok(Self(Key::GamepadMenu)),
            #[cfg(target_os = "windows")]
            "GamepadRightShoulder" => Ok(Self(Key::GamepadRightShoulder)),
            #[cfg(target_os = "windows")]
            "GamepadRightThumbstickButton" => Ok(Self(Key::GamepadRightThumbstickButton)),
            #[cfg(target_os = "windows")]
            "GamepadRightThumbstickDown" => Ok(Self(Key::GamepadRightThumbstickDown)),
            #[cfg(target_os = "windows")]
            "GamepadRightThumbstickLeft" => Ok(Self(Key::GamepadRightThumbstickLeft)),
            #[cfg(target_os = "windows")]
            "GamepadRightThumbstickRight" => Ok(Self(Key::GamepadRightThumbstickRight)),
            #[cfg(target_os = "windows")]
            "GamepadRightThumbstickUp" => Ok(Self(Key::GamepadRightThumbstickUp)),
            #[cfg(target_os = "windows")]
            "GamepadRightTrigger" => Ok(Self(Key::GamepadRightTrigger)),
            #[cfg(target_os = "windows")]
            "GamepadView" => Ok(Self(Key::GamepadView)),
            #[cfg(target_os = "windows")]
            "GamepadX" => Ok(Self(Key::GamepadX)),
            #[cfg(target_os = "windows")]
            "GamepadY" => Ok(Self(Key::GamepadY)),
            #[cfg(target_os = "windows")]
            "Hangeul" => Ok(Self(Key::Hangeul)),
            #[cfg(any(target_os = "windows", all(unix, not(target_os = "macos"))))]
            "Hangul" => Ok(Self(Key::Hangul)),
            #[cfg(any(target_os = "windows", all(unix, not(target_os = "macos"))))]
            "Hanja" => Ok(Self(Key::Hanja)),
            "Help" => Ok(Self(Key::Help)),
            "Home" => Ok(Self(Key::Home)),
            #[cfg(target_os = "windows")]
            "Ico00" => Ok(Self(Key::Ico00)),
            #[cfg(target_os = "windows")]
            "IcoClear" => Ok(Self(Key::IcoClear)),
            #[cfg(target_os = "windows")]
            "IcoHelp" => Ok(Self(Key::IcoHelp)),
            #[cfg(target_os = "macos")]
            "IlluminationDown" => Ok(Self(Key::IlluminationDown)),
            #[cfg(target_os = "macos")]
            "IlluminationUp" => Ok(Self(Key::IlluminationUp)),
            #[cfg(target_os = "macos")]
            "IlluminationToggle" => Ok(Self(Key::IlluminationToggle)),
            #[cfg(target_os = "windows")]
            "IMEOff" => Ok(Self(Key::IMEOff)),
            #[cfg(target_os = "windows")]
            "IMEOn" => Ok(Self(Key::IMEOn)),
            #[cfg(any(target_os = "windows", all(unix, not(target_os = "macos"))))]
            "Insert" => Ok(Self(Key::Insert)),
            #[cfg(target_os = "windows")]
            "Junja" => Ok(Self(Key::Junja)),
            #[cfg(target_os = "windows")]
            "Kana" => Ok(Self(Key::Kana)),
            #[cfg(any(target_os = "windows", all(unix, not(target_os = "macos"))))]
            "Kanji" => Ok(Self(Key::Kanji)),
            #[cfg(target_os = "windows")]
            "LaunchApp1" => Ok(Self(Key::LaunchApp1)),
            #[cfg(target_os = "windows")]
            "LaunchApp2" => Ok(Self(Key::LaunchApp2)),
            #[cfg(target_os = "windows")]
            "LaunchMail" => Ok(Self(Key::LaunchMail)),
            #[cfg(target_os = "windows")]
            "LaunchMediaSelect" => Ok(Self(Key::LaunchMediaSelect)),
            #[cfg(target_os = "macos")]
            "Launchpad" => Ok(Self(Key::Launchpad)),
            #[cfg(target_os = "macos")]
            "LaunchPanel" => Ok(Self(Key::LaunchPanel)),
            #[cfg(target_os = "windows")]
            "LButton" => Ok(Self(Key::LButton)),
            "LControl" => Ok(Self(Key::LControl)),
            "LeftArrow" => Ok(Self(Key::LeftArrow)),
            #[cfg(all(unix, not(target_os = "macos")))]
            "Linefeed" => Ok(Self(Key::Linefeed)),
            #[cfg(any(target_os = "windows", all(unix, not(target_os = "macos"))))]
            "LMenu" => Ok(Self(Key::LMenu)),
            "LShift" => Ok(Self(Key::LShift)),
            #[cfg(target_os = "windows")]
            "LWin" => Ok(Self(Key::LWin)),
            #[cfg(target_os = "windows")]
            "MButton" => Ok(Self(Key::MButton)),
            #[cfg(target_os = "macos")]
            "MediaFast" => Ok(Self(Key::MediaFast)),
            "MediaNextTrack" => Ok(Self(Key::MediaNextTrack)),
            "MediaPlayPause" => Ok(Self(Key::MediaPlayPause)),
            "MediaPrevTrack" => Ok(Self(Key::MediaPrevTrack)),
            #[cfg(target_os = "macos")]
            "MediaRewind" => Ok(Self(Key::MediaRewind)),
            #[cfg(any(target_os = "windows", all(unix, not(target_os = "macos"))))]
            "MediaStop" => Ok(Self(Key::MediaStop)),
            "Meta" | "Super" | "Windows" | "Command" | "Cmd" => Ok(Self(Key::Meta)),
            #[cfg(target_os = "macos")]
            "MissionControl" => Ok(Self(Key::MissionControl)),
            #[cfg(any(target_os = "windows", all(unix, not(target_os = "macos"))))]
            "ModeChange" => Ok(Self(Key::ModeChange)),
            "Multiply" => Ok(Self(Key::Multiply)),
            #[cfg(target_os = "windows")]
            "NavigationAccept" => Ok(Self(Key::NavigationAccept)),
            #[cfg(target_os = "windows")]
            "NavigationCancel" => Ok(Self(Key::NavigationCancel)),
            #[cfg(target_os = "windows")]
            "NavigationDown" => Ok(Self(Key::NavigationDown)),
            #[cfg(target_os = "windows")]
            "NavigationLeft" => Ok(Self(Key::NavigationLeft)),
            #[cfg(target_os = "windows")]
            "NavigationMenu" => Ok(Self(Key::NavigationMenu)),
            #[cfg(target_os = "windows")]
            "NavigationRight" => Ok(Self(Key::NavigationRight)),
            #[cfg(target_os = "windows")]
            "NavigationUp" => Ok(Self(Key::NavigationUp)),
            #[cfg(target_os = "windows")]
            "NavigationView" => Ok(Self(Key::NavigationView)),
            #[cfg(target_os = "windows")]
            "NoName" => Ok(Self(Key::NoName)),
            #[cfg(target_os = "windows")]
            "NonConvert" => Ok(Self(Key::NonConvert)),
            #[cfg(target_os = "windows")]
            "None" => Ok(Self(Key::None)),
            #[cfg(any(target_os = "windows", all(unix, not(target_os = "macos"))))]
            "Numlock" => Ok(Self(Key::Numlock)),
            "Numpad0" => Ok(Self(Key::Numpad0)),
            "Numpad1" => Ok(Self(Key::Numpad1)),
            "Numpad2" => Ok(Self(Key::Numpad2)),
            "Numpad3" => Ok(Self(Key::Numpad3)),
            "Numpad4" => Ok(Self(Key::Numpad4)),
            "Numpad5" => Ok(Self(Key::Numpad5)),
            "Numpad6" => Ok(Self(Key::Numpad6)),
            "Numpad7" => Ok(Self(Key::Numpad7)),
            "Numpad8" => Ok(Self(Key::Numpad8)),
            "Numpad9" => Ok(Self(Key::Numpad9)),
            #[cfg(target_os = "windows")]
            "OEM1" => Ok(Self(Key::OEM1)),
            #[cfg(target_os = "windows")]
            "OEM102" => Ok(Self(Key::OEM102)),
            #[cfg(target_os = "windows")]
            "OEM2" => Ok(Self(Key::OEM2)),
            #[cfg(target_os = "windows")]
            "OEM3" => Ok(Self(Key::OEM3)),
            #[cfg(target_os = "windows")]
            "OEM4" => Ok(Self(Key::OEM4)),
            #[cfg(target_os = "windows")]
            "OEM5" => Ok(Self(Key::OEM5)),
            #[cfg(target_os = "windows")]
            "OEM6" => Ok(Self(Key::OEM6)),
            #[cfg(target_os = "windows")]
            "OEM7" => Ok(Self(Key::OEM7)),
            #[cfg(target_os = "windows")]
            "OEM8" => Ok(Self(Key::OEM8)),
            #[cfg(target_os = "windows")]
            "OEMAttn" => Ok(Self(Key::OEMAttn)),
            #[cfg(target_os = "windows")]
            "OEMAuto" => Ok(Self(Key::OEMAuto)),
            #[cfg(target_os = "windows")]
            "OEMAx" => Ok(Self(Key::OEMAx)),
            #[cfg(target_os = "windows")]
            "OEMBacktab" => Ok(Self(Key::OEMBacktab)),
            #[cfg(target_os = "windows")]
            "OEMClear" => Ok(Self(Key::OEMClear)),
            #[cfg(target_os = "windows")]
            "OEMComma" => Ok(Self(Key::OEMComma)),
            #[cfg(target_os = "windows")]
            "OEMCopy" => Ok(Self(Key::OEMCopy)),
            #[cfg(target_os = "windows")]
            "OEMCusel" => Ok(Self(Key::OEMCusel)),
            #[cfg(target_os = "windows")]
            "OEMEnlw" => Ok(Self(Key::OEMEnlw)),
            #[cfg(target_os = "windows")]
            "OEMFinish" => Ok(Self(Key::OEMFinish)),
            #[cfg(target_os = "windows")]
            "OEMFJJisho" => Ok(Self(Key::OEMFJJisho)),
            #[cfg(target_os = "windows")]
            "OEMFJLoya" => Ok(Self(Key::OEMFJLoya)),
            #[cfg(target_os = "windows")]
            "OEMFJMasshou" => Ok(Self(Key::OEMFJMasshou)),
            #[cfg(target_os = "windows")]
            "OEMFJRoya" => Ok(Self(Key::OEMFJRoya)),
            #[cfg(target_os = "windows")]
            "OEMFJTouroku" => Ok(Self(Key::OEMFJTouroku)),
            #[cfg(target_os = "windows")]
            "OEMJump" => Ok(Self(Key::OEMJump)),
            #[cfg(target_os = "windows")]
            "OEMMinus" => Ok(Self(Key::OEMMinus)),
            #[cfg(target_os = "windows")]
            "OEMNECEqual" => Ok(Self(Key::OEMNECEqual)),
            #[cfg(target_os = "windows")]
            "OEMPA1" => Ok(Self(Key::OEMPA1)),
            #[cfg(target_os = "windows")]
            "OEMPA2" => Ok(Self(Key::OEMPA2)),
            #[cfg(target_os = "windows")]
            "OEMPA3" => Ok(Self(Key::OEMPA3)),
            #[cfg(target_os = "windows")]
            "OEMPeriod" => Ok(Self(Key::OEMPeriod)),
            #[cfg(target_os = "windows")]
            "OEMPlus" => Ok(Self(Key::OEMPlus)),
            #[cfg(target_os = "windows")]
            "OEMReset" => Ok(Self(Key::OEMReset)),
            #[cfg(target_os = "windows")]
            "OEMWsctrl" => Ok(Self(Key::OEMWsctrl)),
            "Option" => Ok(Self(Key::Option)),
            #[cfg(target_os = "windows")]
            "PA1" => Ok(Self(Key::PA1)),
            #[cfg(target_os = "windows")]
            "Packet" => Ok(Self(Key::Packet)),
            "PageDown" => Ok(Self(Key::PageDown)),
            "PageUp" => Ok(Self(Key::PageUp)),
            #[cfg(any(target_os = "windows", all(unix, not(target_os = "macos"))))]
            "Pause" => Ok(Self(Key::Pause)),
            #[cfg(target_os = "windows")]
            "Play" => Ok(Self(Key::Play)),
            #[cfg(target_os = "macos")]
            "Power" => Ok(Self(Key::Power)),
            #[cfg(any(target_os = "windows", all(unix, not(target_os = "macos"))))]
            "PrintScr" | "Print" | "Snapshot" => Ok(Self(Key::PrintScr)),
            #[cfg(target_os = "windows")]
            "Processkey" => Ok(Self(Key::Processkey)),
            #[cfg(target_os = "windows")]
            "RButton" => Ok(Self(Key::RButton)),
            #[cfg(target_os = "macos")]
            "RCommand" => Ok(Self(Key::RCommand)),
            "RControl" => Ok(Self(Key::RControl)),
            #[cfg(all(unix, not(target_os = "macos")))]
            "Redo" => Ok(Self(Key::Redo)),
            "Return" => Ok(Self(Key::Return)),
            "RightArrow" => Ok(Self(Key::RightArrow)),
            #[cfg(target_os = "windows")]
            "RMenu" => Ok(Self(Key::RMenu)),
            #[cfg(target_os = "macos")]
            "ROption" => Ok(Self(Key::ROption)),
            "RShift" => Ok(Self(Key::RShift)),
            #[cfg(target_os = "windows")]
            "RWin" => Ok(Self(Key::RWin)),
            #[cfg(target_os = "windows")]
            "Scroll" => Ok(Self(Key::Scroll)),
            #[cfg(all(unix, not(target_os = "macos")))]
            "ScrollLock" => Ok(Self(Key::ScrollLock)),
            #[cfg(any(target_os = "windows", all(unix, not(target_os = "macos"))))]
            "Select" => Ok(Self(Key::Select)),
            #[cfg(all(unix, not(target_os = "macos")))]
            "ScriptSwitch" => Ok(Self(Key::ScriptSwitch)),
            #[cfg(target_os = "windows")]
            "Separator" => Ok(Self(Key::Separator)),
            "Shift" => Ok(Self(Key::Shift)),
            #[cfg(all(unix, not(target_os = "macos")))]
            "ShiftLock" => Ok(Self(Key::ShiftLock)),
            #[cfg(target_os = "windows")]
            "Sleep" => Ok(Self(Key::Sleep)),
            #[cfg(target_os = "windows")]
            "Space" => Ok(Self(Key::Space)),
            "Subtract" => Ok(Self(Key::Subtract)),
            #[cfg(all(unix, not(target_os = "macos")))]
            "SysReq" => Ok(Self(Key::SysReq)),
            "Tab" => Ok(Self(Key::Tab)),
            #[cfg(all(unix, not(target_os = "macos")))]
            "Undo" => Ok(Self(Key::Undo)),
            "UpArrow" => Ok(Self(Key::UpArrow)),
            #[cfg(target_os = "macos")]
            "VidMirror" => Ok(Self(Key::VidMirror)),
            "VolumeDown" => Ok(Self(Key::VolumeDown)),
            "VolumeMute" => Ok(Self(Key::VolumeMute)),
            "VolumeUp" => Ok(Self(Key::VolumeUp)),
            #[cfg(all(unix, not(target_os = "macos")))]
            "MicMute" => Ok(Self(Key::MicMute)),
            #[cfg(target_os = "windows")]
            "XButton1" => Ok(Self(Key::XButton1)),
            #[cfg(target_os = "windows")]
            "XButton2" => Ok(Self(Key::XButton2)),
            #[cfg(target_os = "windows")]
            "Zoom" => Ok(Self(Key::Zoom)),
            _ => {
                if let Some(s) = s.strip_prefix("0x") {
                    u32::from_str_radix(s, 16)
                        .map(|n| Self(Key::Other(n)))
                        .map_err(|_| ())
                } else {
                    let mut chars = s.chars();
                    if let Some(ch) = chars.next()
                        && chars.next().is_none()
                    {
                        Ok(Self(Key::Unicode(ch)))
                    } else {
                        Err(())
                    }
                }
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct MouseButton(pub Button);

impl MouseButton {
    #[inline]
    pub const fn to_ne_bytes(self) -> [u8; std::mem::size_of::<Button>()] {
        unsafe { std::mem::transmute(self) }
    }

    #[inline]
    pub const fn from_ne_bytes(value: [u8; std::mem::size_of::<Button>()]) -> Self {
        unsafe { std::mem::transmute(value) }
    }
}

impl From<Button> for MouseButton {
    #[inline]
    fn from(value: Button) -> Self {
        Self(value)
    }
}

impl From<MouseButton> for Button {
    #[inline]
    fn from(value: MouseButton) -> Self {
        value.0
    }
}

impl std::str::FromStr for MouseButton {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Left" => Ok(Self(Button::Left)),
            "Middle" => Ok(Self(Button::Middle)),
            "Right" => Ok(Self(Button::Right)),
            "Back" => Ok(Self(Button::Back)),
            "Forward" => Ok(Self(Button::Forward)),
            "ScrollUp" => Ok(Self(Button::ScrollUp)),
            "ScrollDown" => Ok(Self(Button::ScrollDown)),
            "ScrollLeft" => Ok(Self(Button::ScrollLeft)),
            "ScrollRight" => Ok(Self(Button::ScrollRight)),
            _ => Err(()),
        }
    }
}
