use image::{ImageBuffer, RgbImage};

fn new_image() -> RgbImage {
    ImageBuffer::new(512, 512)
}

/* 
 * Returns a 512 x 512 pixel RgbImage of the Mandelbrot set.
 * You may call new_image() above to get an empty image to fill in.
 * See documentation: https://docs.rs/crate/image/0.23.14
 * 
 * Map the number of iterations in a linear fashion to grayscale; the result is the color of each pixel.
 * Use x in [-2.5, 1] and y in [-1, 1].
 * We encourage you to translate the pseudocode at https://en.wikipedia.org/wiki/Mandelbrot_set.
 * The pseudocode is replicated below in case someone edits Wikipedia during the project.
 * 
 * for each pixel (Px, Py) on the screen do
 *   x0 := scaled x coordinate of pixel (scaled to lie in the Mandelbrot X scale (-2.5, 1))
 *   y0 := scaled y coordinate of pixel (scaled to lie in the Mandelbrot Y scale (-1, 1))
 *   x := 0.0
 *   y := 0.0
 *   iteration := 0
 *   max_iteration := 1000
 *   while (x*x + y*y ≤ 2*2 AND iteration < max_iteration) do
 *       xtemp := x*x - y*y + x0
 *       y := 2*x*y + y0
 *       x := xtemp
 *       iteration := iteration + 1
 *   
 *   color := palette[iteration]
 *   plot(Px, Py, color)
 * 
 */
pub fn make_mandelbrot_image() -> RgbImage {
    let mut image = new_image();
    let (_width, _height) = image.dimensions();

    // Hint: make `image` mut and then modify it.
    for i in 0.._width {
        for j in 0.._height {
            let x0 = (i as f64) / (_width as f64) * (1.0 + 2.5) - 2.5;
            let y0 = (j as f64) / (_height as f64) * (1.0 + 1.0) - 1.0;
            let mut x = 0.0;
            let mut y = 0.0;
            let mut iteration = 0;
            let max_iteration = 1000;

            while x*x + y*y <= 2.0 * 2.0 && iteration < max_iteration {
                let xtemp = x*x - y*y + x0;
                y = 2.0 * x * y + y0;
                x = xtemp;
                iteration += 1;
            }
            let color = ((iteration as f64) / 1000.0 * 255.0) as u8;
            let pixel = image::Rgb([color, color, color]);
            image.put_pixel(i, j, pixel);
        }
    }

    return image;
}