---
aliases:
- /es/elixir/working-with-json/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:04.343635-07:00
description: "Trabajar con JSON implica analizar cadenas con formato JSON en estructuras\
  \ de datos que Elixir puede manipular y serializar estructuras de datos de Elixir\u2026"
lastmod: 2024-02-18 23:09:09.680202
model: gpt-4-0125-preview
summary: "Trabajar con JSON implica analizar cadenas con formato JSON en estructuras\
  \ de datos que Elixir puede manipular y serializar estructuras de datos de Elixir\u2026"
title: Trabajando con JSON
---

{{< edit_this_page >}}

## Qué y Por Qué?

Trabajar con JSON implica analizar cadenas con formato JSON en estructuras de datos que Elixir puede manipular y serializar estructuras de datos de Elixir de nuevo en cadenas JSON. Esto es esencial para el desarrollo web, las API y los archivos de configuración, ya que JSON es un formato de intercambio de datos ligero, basado en texto, independiente del lenguaje y ampliamente utilizado por su simplicidad y legibilidad humana.

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
