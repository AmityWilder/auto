use windows::Win32::{
    Foundation::{COLORREF, HWND},
    Graphics::Gdi::{CLR_INVALID, GetDC, GetPixel, HDC, ReleaseDC},
};

#[derive(Debug)]
pub struct Hdc(Option<HWND>, HDC);

impl Drop for Hdc {
    fn drop(&mut self) {
        let mut tmp = std::mem::MaybeUninit::uninit();
        unsafe {
            std::ptr::copy_nonoverlapping(self, tmp.as_mut_ptr(), 1);
            tmp.assume_init().release().unwrap();
        }
    }
}

impl Hdc {
    /// The `GetDC` function retrieves a handle to a device context (DC) for the client area of a specified window or for the entire
    /// screen. You can use the returned handle in subsequent GDI functions to draw in the DC. The device context is an opaque
    /// data structure, whose values are used internally by GDI.
    ///
    /// The `GetDCEx` function is an extension to `GetDC`, which gives an application more control over how and whether clipping
    /// occurs in the client area.
    ///
    /// # Syntax
    /// ```ignore
    /// HDC GetDC(
    ///   [in] HWND hWnd
    /// );
    /// ```
    ///
    /// # Parameters
    ///
    /// `[in] hWnd`
    ///
    /// A handle to the window whose DC is to be retrieved. If this value is `NULL`, `GetDC` retrieves the DC for the entire screen.
    ///
    /// # Return value
    ///
    /// If the function succeeds, the return value is a handle to the DC for the specified window's client area.
    ///
    /// If the function fails, the return value is `NULL`.
    ///
    /// # Remarks
    ///
    /// The `GetDC` function retrieves a common, class, or private DC depending on the class style of the specified window. For
    /// class and private DCs, `GetDC` leaves the previously assigned attributes unchanged. However, for common DCs, `GetDC`
    /// assigns default attributes to the DC each time it is retrieved. For example, the default font is System, which is a bitmap
    /// font. Because of this, the handle to a common DC returned by `GetDC` does not tell you what font, color, or brush was
    /// used when the window was drawn. To determine the font, call `GetTextFace`.
    ///
    /// Note that the handle to the DC can only be used by a single thread at any one time.
    ///
    /// After painting with a common DC, the `ReleaseDC` function must be called to release the DC. Class and private DCs do
    /// not have to be released. `ReleaseDC` must be called from the same thread that called GetDC. The number of DCs is
    /// limited only by available memory.
    pub unsafe fn new(hwnd: Option<HWND>) -> Option<Self> {
        let hdc = unsafe { GetDC(hwnd) };
        (!hdc.is_invalid()).then(|| Self(hwnd, hdc))
    }

    /// The `ReleaseDC` function releases a device context (DC), freeing it for use by other applications. The effect of the
    /// `ReleaseDC` function depends on the type of DC. It frees only common and window DCs. It has no effect on class or
    /// private DCs.
    ///
    /// # Syntax
    ///
    /// ```ignore
    /// int ReleaseDC(
    ///   [in] HWND hWnd,
    ///   [in] HDC  hDC
    /// );
    /// ```
    ///
    /// # Parameters
    ///
    /// `[in] hWnd`
    ///
    /// A handle to the window whose DC is to be released.
    ///
    /// `[in] hDC`
    ///
    /// A handle to the DC to be released.
    ///
    /// # Return value
    ///
    /// The return value indicates whether the DC was released. If the DC was released, the return value is 1.
    ///
    /// If the DC was not released, the return value is zero.
    ///
    /// # Remarks
    ///
    /// The application must call the `ReleaseDC` function for each call to the `GetWindowDC` function and for each call to the
    /// `GetDC` function that retrieves a common DC.
    ///
    /// An application cannot use the `ReleaseDC` function to release a DC that was created by calling the `CreateDC` function;
    /// instead, it must use the `DeleteDC` function. `ReleaseDC` must be called from the same thread that called `GetDC`.
    pub unsafe fn release(self) -> Option<()> {
        let (hwnd, hdc) = (self.0, self.1);
        std::mem::forget(self);
        (unsafe { ReleaseDC(hwnd, hdc) } == 1).then_some(())
    }

    /// The GetPixel function retrieves the red, green, blue (RGB) color value of the pixel at the specified coordinates.
    ///
    /// # Syntax
    ///
    /// ```ignore
    /// COLORREF GetPixel(
    ///   [in] HDC hdc,
    ///   [in] int x,
    ///   [in] int y
    /// );
    /// ```
    ///
    /// Parameters
    ///
    /// `[in] hdc`
    ///
    /// A handle to the device context.
    ///
    /// `[in] x`
    ///
    /// The x-coordinate, in logical units, of the pixel to be examined.
    ///
    /// `[in] y`
    ///
    /// The y-coordinate, in logical units, of the pixel to be examined.
    ///
    /// # Return value
    ///
    /// The return value is the `COLORREF` value that specifies the RGB of the pixel. If the pixel is outside of the current clipping
    /// region, the return value is `CLR_INVALID` (0xFFFFFFFF defined in Wingdi.h).
    ///
    /// # Remarks
    ///
    /// The pixel must be within the boundaries of the current clipping region.
    ///
    /// Not all devices support `GetPixel`. An application should call `GetDeviceCaps` to determine whether a specified device
    /// supports this function.
    ///
    /// A bitmap must be selected within the device context, otherwise, `CLR_INVALID` is returned on all pixels.
    pub unsafe fn get_pixel(&mut self, x: i32, y: i32) -> Option<ColorRef> {
        match unsafe { GetPixel(self.1, x, y) } {
            COLORREF(CLR_INVALID) => None,
            color => Some(ColorRef(color.0)),
        }
    }
}

pub type Screen = Hdc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct ColorRef(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct ColorRGB {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl std::str::FromStr for ColorRGB {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_ascii() && s.len() == 7 && s.starts_with('#') {
            Ok(Self {
                r: u8::from_str_radix(&s[1..3], 16).map_err(|_| ())?,
                g: u8::from_str_radix(&s[3..5], 16).map_err(|_| ())?,
                b: u8::from_str_radix(&s[5..7], 16).map_err(|_| ())?,
            })
        } else {
            Err(())
        }
    }
}

impl ColorRef {
    #[inline]
    pub const fn is_invalid(self) -> bool {
        self.0 == CLR_INVALID
    }

    #[inline]
    pub const fn to_rgb(self) -> ColorRGB {
        let [r, g, b, _x] = self.0.to_le_bytes();
        debug_assert!(self.is_invalid() || _x == 0);
        ColorRGB { r, g, b }
    }
}
