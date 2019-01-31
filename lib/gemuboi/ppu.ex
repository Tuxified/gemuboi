defmodule Gemuboi.Ppu do
  @moduledoc """
  Module representing the Pixel Processing Unit

  Important addresses:

  Address | Register            | Stats
  ------------------------------------------
  0xFF40  | LCD and PPU control | Read/write
  0xFF42  | Scroll-Y            | Read/write
  0xFF43  | Scroll-X            | Read/write
  0xFF44  | Current scan line   | Read only
  0xFF47  | Background palette  | Write only
  ------------------------------------------

  LCD & PPU comtrol byte:

  Bit | Function                | When 0  | When 1
  -------------------------------------------------
  0   | Background: on/off      | Off     | On
  1   | Sprites: on/off         | Off     | On
  2   | Sprites: size (pixels)  | 8x8     | 8x16
  3   | Background: tile map    | #0      | #1
  4   | Background: tile set    | #0      | #1
  5   | Window: on/off          | Off     | On
  6   | Window: tile map        | #0      | #1
  7   | Display: on/off         | Off     | On
  -------------------------------------------------

  Refresh rate is 59.73Hz - 16742µs
  """

  defmodule Sprite do
    @moduledoc """
    Each sprite uses 4 bytes of mem:

    Byte  |   Description
    -------------------------------------------------
    0     |   Y-coordinate of top-left corner
          |   (Value stored is Y-coordinate minus 16)
    1     |   X-coordinate of top-left corner
          |   (Value stored is X-coordinate minus 8)
    2     |   Data tile number
    3     |   Options:
    -------------------------------------------------
            Bit | Description       | When 0  | When 1
    -------------------------------------------------
            7   | Sprite priority   |  Above  | Below background (except colour 0)
            6   | Y-flip            | Normal  | Vertically flipped
            5   | X-flip            | Normal  | Horizontally flipped
            4   | Palette           | #0      | OBJ palette #1
    -------------------------------------------------

    Sprites are located at 0xFE00 - 0xFE9F: OAM (Object Atribute Memory)

    """

    defmodule Options do
      defstruct visible: true, y_flip: false, x_flip: false, palette: 0x0

      def parse(
            <<visible::size(1), y_flip::size(1), x_flip::size(1), palette::size(1),
              _rest::size(4)>>
          ) do
        %Options{
          visible: to_boolean(visible),
          y_flip: to_boolean(y_flip),
          x_flip: to_boolean(x_flip),
          palette: palette
        }
      end

      def to_boolean(0b1), do: true
      def to_boolean(0b0), do: false
    end

    # %Options{}
    defstruct y: 0x0, x: 0x0, tile: 0x0, options: %{}

    def parse(
          <<y::integer-size(1), x::integer-size(1), tile::integer-size(1),
            options::binary-size(1)>>
        ) do
      %Sprite{y: y, x: x, tile: tile, options: Options.parse(options)}
    end
  end

  defmodule Tile do
    @example_data <<0x7C, 0x7C, 0x00, 0xC6, 0xC6, 0x00, 0x00, 0xFE, 0xC6, 0xC6, 0x00, 0xC6, 0xC6,
                    0x00, 0x00, 0x00>>

    # 01111100
    # 01111100
    # 00000000
    # 11000110
    # 11000110
    # 00000000
    # 00000000
    # 11111110
    # 11000110
    # 11000110
    # 00000000
    # 11000110
    # 11000110
    # 00000000
    # 00000000
    # 00000000

    @example_tile """
    .33333..
    22...22.
    11...11.
    2222222.
    33...33.
    22...22.
    11...11.
    ........
    """
    def example do
      # == @example_tile
      translate_bg(@example_data, :ascii)
    end

    def translate_bg(binary, format, result \\ [])

    def translate_bg("", :ascii, result), do: Enum.reverse(result)

    def translate_bg(<<row::binary-size(2), rest::binary>>, :ascii, result) do
      <<a1::size(1), a2::size(1), a3::size(1), a4::size(1), a5::size(1), a6::size(1), a7::size(1),
        a8::size(1), b1::size(1), b2::size(1), b3::size(1), b4::size(1), b5::size(1), b6::size(1),
        b7::size(1), b8::size(1)>> = row

      new_pixels = [
        pixel(a1, b1),
        pixel(a2, b2),
        pixel(a3, b3),
        pixel(a4, b4),
        pixel(a5, b5),
        pixel(a6, b6),
        pixel(a7, b7),
        pixel(a8, b8)
      ]

      translate_bg(rest, :ascii, [new_pixels | result])
    end

    def translate_bg("", :pixels, result), do: Enum.reverse(result)

    def translate_bg(<<row::binary-size(2), rest::binary>>, :pixels, result) do
      <<a1::size(1), a2::size(1), a3::size(1), a4::size(1), a5::size(1), a6::size(1), a7::size(1),
        a8::size(1), b1::size(1), b2::size(1), b3::size(1), b4::size(1), b5::size(1), b6::size(1),
        b7::size(1), b8::size(1)>> = row

      new_pixels = [
        color(a1, b1),
        color(a2, b2),
        color(a3, b3),
        color(a4, b4),
        color(a5, b5),
        color(a6, b6),
        color(a7, b7),
        color(a8, b8)
      ]

      translate_bg(rest, :pixels, [new_pixels | result])
    end

    def pixel(1, 1), do: "3"
    def pixel(1, 0), do: "2"
    def pixel(0, 1), do: "1"
    def pixel(0, 0), do: "."

    def color(1, 1), do: {8, 24, 32}
    def color(1, 0), do: {52, 104, 86}
    def color(0, 1), do: {136, 192, 112}
    def color(0, 0), do: {231, 255, 208}
  end
end
