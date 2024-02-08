---
title:                "Trabalhando com JSON"
aliases:
- pt/elixir/working-with-json.md
date:                  2024-02-03T19:22:09.916314-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?

Trabalhar com JSON envolve analisar strings formatadas em JSON em estruturas de dados que o Elixir pode manipular e serializar estruturas de dados do Elixir de volta para strings JSON. Isso é essencial para o desenvolvimento web, APIs e arquivos de configuração, pois o JSON é um formato de troca de dados leve, baseado em texto, independente de linguagem, amplamente utilizado por sua simplicidade e legibilidade para humanos.

## Como fazer:

No Elixir, você pode usar a biblioteca `Jason`, uma escolha popular para análise e geração de JSON. Primeiro, adicione `Jason` às dependências do seu projeto em `mix.exs`:

```elixir
defp deps do
  [
    {:jason, "~> 1.3"}
  ]
end
```

Depois, execute `mix deps.get` para buscar a dependência.

### Analisando JSON:
Para converter uma string JSON em estruturas de dados do Elixir:

```elixir
json_string = "{\"name\":\"John\", \"age\":30}"
{:ok, person} = Jason.decode(json_string)
IO.inspect(person)
# Saída: %{"name" => "John", "age" => 30}
```

### Gerando JSON:
Para converter um map do Elixir em uma string JSON:

```elixir
person_map = %{"name" => "Jane", "age" => 25}
{:ok, json_string} = Jason.encode(person_map)
IO.puts(json_string)
# Saída: {"age":25,"name":"Jane"}
```

### Trabalhando com Structs:
Para codificar uma struct do Elixir, você deve implementar o protocolo `Jason.Encoder` para a sua struct. Aqui está um exemplo:

```elixir
defmodule Person do
  @derive {Jason.Encoder, only: [:name, :age]}
  defstruct name: nil, age: nil
end

person_struct = %Person{name: "Mike", age: 28}
{:ok, json_string} = Jason.encode(person_struct)
IO.puts(json_string)
# Saída: {"age":28,"name":"Mike"}
```

Esta abordagem simples ajudará você a começar a integrar o processamento JSON em suas aplicações Elixir, facilitando a troca de dados em diversos ambientes de programação.
