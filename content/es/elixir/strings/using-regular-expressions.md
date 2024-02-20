---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:20.075666-07:00
description: "Las expresiones regulares (regex) en Elixir se utilizan para buscar,\
  \ coincidir y manipular cadenas basadas en patrones espec\xEDficos. Los programadores\u2026"
lastmod: 2024-02-19 22:05:17.279659
model: gpt-4-0125-preview
summary: "Las expresiones regulares (regex) en Elixir se utilizan para buscar, coincidir\
  \ y manipular cadenas basadas en patrones espec\xEDficos. Los programadores\u2026"
title: Usando expresiones regulares
---

{{< edit_this_page >}}

## Qué y Por Qué?

Las expresiones regulares (regex) en Elixir se utilizan para buscar, coincidir y manipular cadenas basadas en patrones específicos. Los programadores aprovechan regex para tareas como validar formatos (correo electrónico, URLs), analizar registros o extracción de datos, gracias a su eficiencia y versatilidad en el manejo de cadenas.

## Cómo hacerlo:

Elixir utiliza el módulo `Regex`, aprovechando la biblioteca regex de Erlang, para operaciones de regex. Aquí están los usos básicos:

```elixir
# Coincidir con un patrón - Devuelve la primera coincidencia
match_result = Regex.run(~r/hello/, "hello world")
IO.inspect(match_result) # Salida: ["hello"]

# Encontrar todas las coincidencias
all_matches = Regex.scan(~r/\d/, "Hay 2 manzanas y 5 naranjas.")
IO.inspect(all_matches) # Salida: [["2"], ["5"]]

# Reemplazar partes de una cadena
replaced_string = Regex.replace(~r/\s+/, "Elixir es divertido", "_")
IO.inspect(replaced_string) # Salida: "Elixir_es_divertido"
```

Para patrones más complejos y funcionalidades, podrías considerar usar bibliotecas de terceros, aunque para la mayoría de las tareas básicas de coincidencia de cadenas y patrones, el módulo `Regex` incorporado en Elixir es bastante poderoso.

Para realizar una coincidencia sin tener en cuenta mayúsculas o minúsculas, usa la opción `i`:

```elixir
case_insensitive_match = Regex.run(~r/hello/i, "Hello World")
IO.inspect(case_insensitive_match) # Salida: ["Hello"]
```

Las expresiones Regex pueden precompilarse para aumentar la eficiencia cuando se usan varias veces:

```elixir
precompiled_regex = Regex.compile!("hello")
match_result_precompiled = Regex.run(precompiled_regex, "hello world")
IO.inspect(match_result_precompiled) # Salida: ["hello"]
```

Elixir también soporta capturas nombradas, que pueden ser muy útiles para extraer partes específicas de una cadena mientras hacen tu código más legible:

```elixir
date_string = "2023-04-15"
pattern = ~r/(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})/
{:ok, captures} = Regex.run(pattern, date_string, capture: :all_names)
IO.inspect(captures) # Salida: %{"year" => "2023", "month" => "04", "day" => "15"}
```

Esta breve descripción general subraya la facilidad con la que Elixir maneja las expresiones regulares, permitiendo técnicas potentes de manipulación de cadenas y extracción de datos.
