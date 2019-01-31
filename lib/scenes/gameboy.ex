defmodule Gemuboi.Scene.Gameboy do
  alias Scenic.Graph
  use Scenic.Scene, has_children: false

  alias Gemuboi.Component.{FPS}
  alias Gameboy.Memory

  import Scenic.Primitives
  # import Scenic.Components

  @pixel_size 1

  # "test/tile_map_tetris_menu.rom" # ("test/bg.rom")
  @tile_data File.read!("test/tile_map_tetris_registered.rom")
  # "test/bg_tetris_menu.rom" #("test/bg_map.rom")
  @bg_data File.read!("test/bg_tetris_registered.rom")

  @graph Graph.build(font: :roboto, font_size: 20)

  alias Scenic.Utilities.Texture
  alias Scenic.Cache.Dynamic.Texture, as: Cache

  #  components:
  #   - game itself
  #   - debugger (current opcode, registers, mem?)
  #   - tile and/or background map

  # ============================================================================
  # setup
  # --------------------------------------------------------
  # def init(_first_scene, _opts) do
  # def init({_first_scene, _opts}) do
  #   graph =
  #     @graph
  #     |> group(
  #       fn g ->
  #         g
  #         # |> StaticBackground.add_to_graph()
  #         |> FPS.add_to_graph(__MODULE__, t: {20, 280})
  #       end,
  #       []
  #     )
  #     |> push_graph

  #   {:ok, graph}
  # end

  def init(_args, _opts) do
    # :observer.start()
    gb = Gameboy.init() |> Gameboy.tick()
    start_y = -0x64

    screen = Texture.build!(:rgb, 256 * @pixel_size, 256 * @pixel_size, clear: {155, 188, 15})

    Cache.put("screen", screen)

    graph =
      @graph
      |> rect({256 * @pixel_size, 256 * @pixel_size},
        fill: {:dynamic, "screen"},
        translate: {0, start_y},
        id: :gameboy
      )
      |> text("", id: :fps, translate: {20 * @pixel_size, 70 * @pixel_size})

    {:ok, timer} = :timer.send_interval(trunc(1_000 / 59.73), :animate_frame)

    state = %{
      timer: timer,
      gb: gb,
      graph: graph,
      cpu_running: false,
      y: start_y,
      last: Time.utc_now(),
      start: Time.utc_now(),
      frame: 0,
      screen: screen
    }

    send(self(), :tick)

    {:ok, state, push: graph}
  end

  def handle_info(:tick, state) do
    gb_new = Gameboy.tick(state.gb)
    scroll_y = -1 * Memory.read8(gb_new.memory, 0xFF42)
    scroll_x = -1 * Memory.read8(gb_new.memory, 0xFF43)

    new_graph =
      state.graph
      |> Graph.modify(:gameboy, &update_opts(&1, translate: {scroll_x, scroll_y}))

    send(self(), :tick)
    {:noreply, %{state | gb: gb_new}, push: new_graph}
  end

  def handle_info(:animate_frame, state) do
    if state.gb.cycles <= 70224 do
      gb_new = Gameboy.tick(state.gb)

      {:noreply, %{state | gb: gb_new}}
    else
      gb_new = Gameboy.tick(%{state.gb | cycles: 0, running: true})

      lcd_flags = parse_lcd_byte(<<Memory.read8(gb_new.memory, 0xFF40)>>)

      tiles =
          case lcd_flags[:background_tile_set] do
            1 ->
              tiles(:binary.part(gb_new.memory.data, 0x8000, 0x1000))

            0 ->
              tiles(:binary.part(gb_new.memory.data, 0x8800, 0x1000))
          end

      bg =
        case lcd_flags[:background_tile_map] do
          0 ->
            bg(tiles, :binary.part(gb_new.memory.data, 0x9800, 0x400))

          1 ->
            bg(tiles, :binary.part(gb_new.memory.data, 0x9C00, 0x400))
        end

      # tiles = tiles(:binary.part(gb_new.memory.data, 0x8000, 0x1000))
      # bg = bg(tiles, :binary.part(gb_new.memory.data, 0x9800, 0x400))

      # tiles = tiles(:binary.part(gb_new.memory.data, 0x8000, 0x1800))
      # scroll_y = -1 * Memory.read8(gb_new.memory, 0xFF42)
      # scroll_x = -1 * Memory.read8(gb_new.memory, 0xFF43)
      # spawn(fn -> generate_png_tiles(tiles) end)
      screen =
        Enum.reduce(Stream.zip(0..0x400, bg), state.screen, fn {it, tile}, tps ->
          Enum.reduce(Stream.zip(Stream.cycle(Enum.to_list(0..7)), tile), tps, fn {ir, row}, rps ->
            Enum.reduce(Stream.zip(Stream.cycle(Enum.to_list(0..7)), row), rps, fn {ip, px}, pxs ->
              y = div(it, 32) * 8 * @pixel_size + ir * @pixel_size
              x = rem(it, 32) * 8 * @pixel_size + ip * @pixel_size
              Texture.put!(pxs, x, y, px)
            end)
          end)
        end)

      Cache.put("screen", screen)

      # new_graph =
      #   state.graph
      #   |> Graph.modify(:gameboy, &update_opts(&1, translate: {scroll_x, scroll_y}))

      # |> Graph.modify(:gameboy, &render_bg(&1, bg))

      {:noreply, %{state | gb: gb_new, screen: screen}}
    end
  end

  # Keyboard controls
  def handle_input({:key, {"left", :press, _}}, _context, state) do
    IO.puts("left pressed")
    {:noreply, state}
  end

  def handle_input({:key, {"right", :press, _}}, _context, state) do
    IO.puts("right pressed")
    {:noreply, state}
  end

  def handle_input({:key, {"up", :press, _}}, _context, state) do
    IO.puts("up pressed")
    {:noreply, state}
  end

  def handle_input({:key, {"down", :press, _}}, _context, state) do
    IO.puts("down pressed")
    {:noreply, state}
  end

  def handle_input({:key, {"A", :press, _}}, _context, state) do
    IO.puts("A pressed")
    {:noreply, state}
  end

  def handle_input({:key, {"S", :press, _}}, _context, state) do
    IO.puts("S pressed")
    {:noreply, state}
  end

  def handle_input({:key, {"L", :press, _}}, _context, state) do
    gb_new = %{state.gb | log: !state.gb.log}

    {:noreply, %{state | gb: gb_new}}
  end

  def handle_input({:key, {"P", :press, _}}, _context, state) do
    gb_new = %{state.gb | running: !state.gb.running}

    new_graph =
      case gb_new.running do
        true ->
          # Scenic.ViewPort.reshape(context.viewport, {160, 144})
          Graph.modify(state.graph, :fps, &text(&1, ""))

        false ->
          # Scenic.ViewPort.reshape(context.viewport, {160, 160})
          Graph.modify(state.graph, :fps, &text(&1, "PAUZED"))
      end

    IO.puts("P pressed")
    IO.puts(inspect(gb_new))
    {:noreply, %{state | gb: gb_new}, push_graph: new_graph}
  end

  def handle_input({:key, {key, :press, _}}, _context, state) do
    IO.puts("#{inspect(key)} pressed")
    {:noreply, state}
  end

  def handle_input(_input, _context, state), do: {:noreply, state}

  def tiles(tile_data \\ @tile_data)

  def tiles(tile_data) do
    pixel_rows =
      for <<tile::binary-size(16) <- tile_data>>, <<row::binary-size(2) <- tile>>,
        do: translate_row(row)

    Enum.chunk_every(pixel_rows, 8)
  end

  # def generate_png_tiles(tiles) do
  #   Stream.cycle(0..0xFFFF)
  #   |> Enum.zip(tiles) #:orddict.from_list()
  #   |> Enum.map(fn {i, pxs} ->
  #     filename = :code.priv_dir(:gemuboi) |> Path.join("/static/images/tile_#{i}.png")
  #     {:ok, file} = :file.open(to_charlist(filename), [:write])
  #     png = :png.create(%{size: {8, 8}, mode: {:rgb, 8}, file: file})
  #     pxs
  #       |> Enum.map(& Enum.map(&1, fn {r,g,b} -> << r, g, b >> end))
  #       |> Enum.each(fn(row) -> :png.append(png, {:row, row}) end)
  #     :ok = :png.close(png)
  #     :ok = :file.close(file)
  #     hash = Scenic.Cache.Hash.file!(filename, :sha )
  #     Scenic.Cache.File.load(filename, hash)
  #     hash
  #   end)
  # end

  def bg(tiles \\ tiles(), bg_data \\ @bg_data)

  def bg(tiles, bg_data) do
    atiles =
      tiles
      # |> generate_png_tiles
      |> :array.from_list()

    bg_data
    |> :binary.bin_to_list()
    |> Enum.map(fn x -> :array.get(x, atiles) end)
  end

  def default_tile do
    empty_pix = {231, 255, 208}
    stream = Stream.cycle([empty_pix])
    Enum.take(stream, 8)
  end

  defp translate_row(
         <<a1::size(1), a2::size(1), a3::size(1), a4::size(1), a5::size(1), a6::size(1),
           a7::size(1), a8::size(1), b1::size(1), b2::size(1), b3::size(1), b4::size(1),
           b5::size(1), b6::size(1), b7::size(1), b8::size(1)>>
       ) do
    [
      color(a1, b1),
      color(a2, b2),
      color(a3, b3),
      color(a4, b4),
      color(a5, b5),
      color(a6, b6),
      color(a7, b7),
      color(a8, b8)
    ]
  end

  def color(1, 1), do: {15, 65, 15}
  def color(1, 0), do: {48, 98, 48}
  def color(0, 1), do: {139, 172, 15}
  def color(0, 0), do: {155, 188, 15}

  def parse_lcd_byte(<<
        display_on::size(1),
        window_tile_map::size(1),
        window_on::size(1),
        background_tile_set::size(1),
        background_tile_map::size(1),
        sprites_size_pixels::size(1),
        sprites_on::size(1),
        background_on::size(1)
      >>) do
    %{
      display_on: display_on,
      window_tile_map: window_tile_map,
      window_on: window_on,
      background_on: background_on,
      background_tile_set: background_tile_set,
      background_tile_map: background_tile_map,
      sprites_size_pixels: sprites_size_pixels,
      sprites_on: sprites_on
    }
  end

  # def color(1, 1), do: {8, 24, 32}
  # def color(1, 0), do: {52, 104, 86}
  # def color(0, 1), do: {136, 192, 112}
  # def color(0, 0), do: {231, 255, 208}
end
