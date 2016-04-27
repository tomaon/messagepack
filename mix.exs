defmodule Messagepack.Mixfile do
  use Mix.Project

  @version File.read!("VERSION") |> String.strip

  def project do
    [
      app: :messagepack,
      version: @version,

      compilers: [:erlang, :elixir, :app],
      build_path: ".mix",

      elixirc_options: [
        {:debug_info, true},
        {:warnings_as_errors, true},
      ],

      erlc_options: [
        :bin_opt_info,
        :debug_info,
        :warnings_as_errors,
        :warn_export_all,
        :warn_export_vars,
        :warn_obsolete_guard,
        :warn_unused_import,
      ],

      test_coverage: [
        output: ".mix/cover",
      ],

      description: "MessagePack for Erlang / Elixir",
      package: package,
    ]
  end

  defp package do
    [
      maintainers: ["Tomohiko AONO"],
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub": "https://github.com/tomaon/messagepack",
      },
      files: [
        "LICENSE", "README.md", "VERSION",
        "lib", "mix.exs", "rebar.config", "src",
      ]
    ]
  end

end
