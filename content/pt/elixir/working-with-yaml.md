---
title:                "Trabalhando com YAML"
date:                  2024-01-19
simple_title:         "Trabalhando com YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?

YAML é um formato de serialização de dados legível por humanos, frequentemente usado para configurações de aplicativos e troca de dados entre linguagens. Programadores usam YAML pela sua simplicidade e legibilidade.

## Como Fazer:

```elixir
# Para trabalhar com YAML em Elixir, adicionaremos a biblioteca "yaml_elixir" no mix.exs
defp deps do
  [
    {:yaml_elixir, "~> 2.5"}
  ]
end

# Carregar e ler um arquivo YAML
yaml_content =
  """
  config:
    language: "Elixir"
    features:
      - "Pattern Matching"
      - "First-Class Functions"
  """

# Uso da biblioteca YamlElixir para ler o conteúdo YAML
parsed_content = YamlElixir.read_from_string!(yaml_content)
IO.inspect(parsed_content)
```
Saída:
```elixir
%{
  "config" => %{
    "language" => "Elixir",
    "features" => ["Pattern Matching", "First-Class Functions"]
  }
}
```

## Aprofundamento

O YAML foi introduzido em 2001 como uma alternativa ao XML e ao JSON, com ênfase na legibilidade. Alternativas incluem JSON, XML e TOML, mas YAML é geralmente escolhido para configuração devido à facilidade de leitura e escrita. Internamente, as bibliotecas de Elixir que trabalham com YAML convertem os dados para estruturas Elixir nativas, como mapas e listas.

## Veja Também

- [YamlElixir (Hex package)](https://hex.pm/packages/yaml_elixir): A página do pacote YamlElixir no Hex.
- [Documentação Oficial do YAML](https://yaml.org): Para entender completamente o padrão YAML.
- [Elixir School](https://elixirschool.com/pt/): Tutoriais sobre Elixir, incluindo trabalhar com formatos de dados externos.
