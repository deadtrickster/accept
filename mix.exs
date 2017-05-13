defmodule Accept.Mixfile do
  use Mix.Project

  def project do
    [app: :accept,
     version: "0.3.2",
     description: description(),
     package: package()]
  end

  defp description do
    """
    Accept header(s) for Erlang/Elixir.
    """
  end

  defp package do
    [build_tools: ["rebar3"],
     maintainers: ["Ilya Khaprov"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/deadtrickster/accept"},
     files: ["bin", "src", "include", "README.md", "LICENSE", "rebar.config"]]
  end
end
