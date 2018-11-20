defmodule Eep48.MixProject do
  use Mix.Project

  def project do
    [
      app: :eep48,
      version: "0.1.0",
      elixir: "~> 1.7",
      deps: deps(),

      # Docs
      name: "Hackney",
      source_url: "https://github.com/benoitc/hackney",
      homepage_url: "https://github.com/benoitc/hackney",
      source_root: "_build/default/lib/hackney",
      docs: [
        # logo: "path/to/logo.png",
        main: "readme",
        extras: ["_build/default/lib/hackney/README.md"],
        source_beam: ["priv/ebin"],
        filter_prefix: false
      ]
    ]
  end

  defp deps do
    [
      {:hackney, path: "priv", compile: false, runtime: false},
      {:ex_doc, github: "tank-bohr/ex_doc", only: :dev, runtime: false}
    ]
  end
end
