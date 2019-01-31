# IEx config for this project

IEx.configure(inspect: [base: :hex])

alias Gameboy.{CPU, Memory}

defmodule Kickstarter do
  def motd do
    IO.puts([
      IO.ANSI.green(),
      """

      ██╗    ██╗███████╗██╗      ██████╗ ██████╗ ███╗   ███╗███████╗    ████████╗ ██████╗ 
      ██║    ██║██╔════╝██║     ██╔════╝██╔═══██╗████╗ ████║██╔════╝    ╚══██╔══╝██╔═══██╗
      ██║ █╗ ██║█████╗  ██║     ██║     ██║   ██║██╔████╔██║█████╗         ██║   ██║   ██║
      ██║███╗██║██╔══╝  ██║     ██║     ██║   ██║██║╚██╔╝██║██╔══╝         ██║   ██║   ██║
      ╚███╔███╔╝███████╗███████╗╚██████╗╚██████╔╝██║ ╚═╝ ██║███████╗       ██║   ╚██████╔╝
       ╚══╝╚══╝ ╚══════╝╚══════╝ ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚══════╝       ╚═╝    ╚═════╝ 
                                                                                          
       ██████╗ ███████╗███╗   ███╗██╗   ██╗██████╗  ██████╗ ██╗                           
      ██╔════╝ ██╔════╝████╗ ████║██║   ██║██╔══██╗██╔═══██╗██║                           
      ██║  ███╗█████╗  ██╔████╔██║██║   ██║██████╔╝██║   ██║██║                           
      ██║   ██║██╔══╝  ██║╚██╔╝██║██║   ██║██╔══██╗██║   ██║██║                           
      ╚██████╔╝███████╗██║ ╚═╝ ██║╚██████╔╝██████╔╝╚██████╔╝██║                           
       ╚═════╝ ╚══════╝╚═╝     ╚═╝ ╚═════╝ ╚═════╝  ╚═════╝ ╚═╝                           
                                                                                          


      """,
      IO.ANSI.reset(),
      """
      """
    ])
  end
end

Kickstarter.motd()