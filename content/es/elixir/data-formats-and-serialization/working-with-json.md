---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:04.343635-07:00
description: "C\xF3mo hacerlo: En Elixir, puedes usar la biblioteca `Jason`, una elecci\xF3\
  n popular para el an\xE1lisis y generaci\xF3n de JSON. Primero, a\xF1ade `Jason`\
  \ a las\u2026"
lastmod: '2024-03-13T22:44:58.724697-06:00'
model: gpt-4-0125-preview
summary: "En Elixir, puedes usar la biblioteca `Jason`, una elecci\xF3n popular para\
  \ el an\xE1lisis y generaci\xF3n de JSON."
title: Trabajando con JSON
weight: 38
---

## Cómo hacerlo:
En Elixir, puedes usar la biblioteca `Jason`, una elección popular para el análisis y generación de JSON. Primero, añade `Jason` a las dependencias de tu proyecto en `mix.exs`:

```elixir
defp deps do
  [
    {:jason, "~> 1.3"}
  ]
end
```

Luego, ejecuta `mix deps.get` para obtener la dependencia.

### Analizando JSON:
Para convertir una cadena JSON en estructuras de datos de Elixir:

```elixir
json_string = "{\"name\":\"John\", \"age\":30}"
{:ok, person} = Jason.decode(json_string)
IO.inspect(person)
# Salida: %{"name" => "John", "age" => 30}
```

### Generando JSON:
Para convertir un mapa de Elixir en una cadena JSON:

```elixir
person_map = %{"name" => "Jane", "age" => 25}
{:ok, json_string} = Jason.encode(person_map)
IO.puts(json_string)
# Salida: {"age":25,"name":"Jane"}
```

### Trabajando con Structs:
Para codificar un struct de Elixir, debes implementar el protocolo `Jason.Encoder` para tu struct. Aquí hay un ejemplo:

```elixir
defmodule Person do
  @derive {Jason.Encoder, only: [:name, :age]}
  defstruct name: nil, age: nil
end

person_struct = %Person{name: "Mike", age: 28}
{:ok, json_string} = Jason.encode(person_struct)
IO.puts(json_string)
# Salida: {"age":28,"name":"Mike"}
```

Este enfoque simple te ayudará a comenzar a integrar el procesamiento de JSON en tus aplicaciones Elixir, facilitando el intercambio de datos en diversos entornos de programación.
