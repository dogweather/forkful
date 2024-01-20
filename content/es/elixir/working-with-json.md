---
title:                "Trabajando con JSON"
html_title:           "Bash: Trabajando con JSON"
simple_title:         "Trabajando con JSON"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Qué es y por qué?
Trabajar con JSON significa lidiar con un formato estándar de intercambio de datos. Los programadores lo usan por su simplicidad y porque es ampliamente aceptado en aplicaciones web y APIs.

## Cómo hacerlo:
Elixir maneja JSON con paquetes externos. El más común es `Jason`. Primero, añádelo a tu `mix.exs`:

```elixir
defp deps do
  [
    {:jason, "~> 1.2"}
  ]
end
```

Luego, en tu código:

```elixir
# Para codificar un mapa a JSON:
map = %{foo: "bar", num: 3}
json = Jason.encode!(map) # "{\"foo\":\"bar\",\"num\":3}"

# Para decodificar JSON a un mapa:
json_string = "{\"foo\":\"bar\",\"num\":3}"
map = Jason.decode!(json_string) # %{foo: "bar", num: 3}
```

## Inmersión Profunda:
JSON, o JavaScript Object Notation, nace en los 2000 para facilitar la comunicación entre cliente y servidor. Alternativas como XML existen, pero JSON prevalece por su legibilidad y eficiencia. Elixir no tiene soporte incorporado para JSON, por lo que depende de librerías como `Jason` o `Poison`. Estas librerías convierten datos entre mapas y listas de Elixir y cadenas JSON.

## Ver También:
- Documentación oficial de `Jason`: https://hexdocs.pm/jason/
- Tutorial de `Poison` para JSON en Elixir: https://hexdocs.pm/poison/readme.html
- Guía de Elixir para trabajar con mapas: https://elixir-lang.org/getting-started/maps-and-dicts.html