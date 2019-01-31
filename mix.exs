defmodule Gemuboi.MixProject do
  use Mix.Project

  def project do
    [
      app: :gemuboi,
      version: "0.1.0",
      elixir: "~> 1.7",
      build_embedded: true,
      start_permanent: Mix.env() == :prod,
      aliases: [test: "test --no-start"],
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      mod: {Gemuboi, []},
      extra_applications: []
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:scenic, git: "https://github.com/boydm/scenic.git"},
      {:scenic_driver_glfw, git: "https://github.com/boydm/scenic_driver_glfw.git"},
      {:micro_timer, "~> 0.1"},
      {:number, ">= 0.0.0"},
      {:png, ">= 0.0.0"}
    ]
  end
end
