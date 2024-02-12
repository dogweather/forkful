---
title:                "Trabalhando com TOML"
aliases: - /pt/elixir/working-with-toml.md
date:                  2024-01-26T04:20:57.740470-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/working-with-toml.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Trabalhar com TOML significa analisar e gerar dados TOML (Tom's Obvious, Minimal Language) usando Elixir. Os programadores o utilizam para manipular arquivos de configuração porque o TOML é legível, fácil de analisar e mapeia bem para uma estrutura de dados do tipo hash.

## Como fazer:
Primeiramente, adicione um analisador TOML às suas dependências do mix. Este exemplo utiliza o `toml-elixir`:

```elixir
def deps do
  [
    {:toml_elixir, "~> 2.0"}
  ]
end
```

Leia um arquivo TOML:

```elixir
{:ok, toml_data} = File.read("config.toml")
{:ok, parsed_data} = TomlElixir.parse(toml_data)
```

Para converter dados de Elixir para TOML:

```elixir
data = %{title: "Exemplo TOML", owner: %{name: "Tom Preston-Werner"}}
toml_string = TomlElixir.encode(data)
```

Saída de exemplo:

```elixir
"title = \"Exemplo TOML\"\n\n[owner]\nname = \"Tom Preston-Werner\"\n"
```

## Aprofundamento
TOML foi criado por Tom Preston-Werner, co-fundador do GitHub, para uso em arquivos de configuração. Ele é projetado para ser mais direto que o XML e mais conciso que o YAML, mantendo a consistência.

Alternativas incluem arquivos JSON, YAML e INI, cada um com suas compensações em legibilidade humana e compatibilidade de estrutura de dados. TOML se destaca por representar claramente dados tabulares e o agrupamento aninhado de dados.

Em Elixir, o tratamento TOML depende de bibliotecas de decodificação e codificação, que transformam strings TOML em mapas Elixir e vice-versa. A análise funciona combinando as regras de sintaxe do TOML e convertendo-as para os tipos de dados do Elixir. A codificação faz o oposto mapeando os tipos de dados do Elixir de volta para a sintaxe TOML válida.

## Veja Também
- Linguagem TOML: https://toml.io/pt/
- Repositório GitHub `toml-elixir`: https://github.com/bitwalker/toml-elixir
- Detalhes do pacote Hex para `toml-elixir`: https://hex.pm/packages/toml_elixir
