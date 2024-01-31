---
title:                "Trabalhando com JSON"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com JSON"
simple_title:         "Trabalhando com JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Trabalhar com JSON significa manipular um formato leve de troca de dados. Programadores usam JSON porque é fácil de ler e escrever para humanos e simples de interpretar e gerar para máquinas.

## How to:
```elixir
# Adicione `jason` como dependência no mix.exs
{:jason, "~> 1.2"}

# Vamos decodificar um JSON
json = "{\"nome\": \"João\", \"idade\": 30}"
{:ok, pessoa} = Jason.decode(json)
IO.inspect(pessoa)  # Saída: %{"idade" => 30, "nome" => "João"}

# Agora, codificar um map para JSON
map = %{"nome" => "Maria", "idade" => 25}
{:ok, json_string} = Jason.encode(map)
IO.puts(json_string)  # Saída: {"idade":25,"nome":"Maria"}
```

## Deep Dive
O formato JSON surgiu dos subconjuntos da linguagem JavaScript, mas é independente de linguagem. Alternativas incluem XML e YAML, mas JSON ganha em simplicidade e velocidade de parse. Em Elixir, a biblioteca `jason` é uma escolha popular por sua alta performance. Outra opção é a `poison`, mas `jason` é geralmente mais rápida e tem sido adotada como preferência em muitos projetos da comunidade.

## See Also
- [Guia oficial da biblioteca Jason](https://hexdocs.pm/jason/readme.html)
- [Documentação de `poison` no HexDocs](https://hexdocs.pm/poison/readme.html)
