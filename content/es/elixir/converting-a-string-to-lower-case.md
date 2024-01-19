---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

---

# Elixir: Cómo Convertir una Cadena de Caracteres a Minúsculas

---

## ¿Qué & Por Qué?

Convertir una cadena de caracteres a minúsculas consiste en cambiar todas las letras mayúsculas que se encuentran en un texto a su correspondiente minúscula. Los programadores lo hacen para normalizar datos y facilitar comparaciones y búsquedas de texto.

## ¿Cómo Hacerlo?

La función `String.downcase/1` en Elixir convierte una cadena de caracteres a minúsculas. Aquí te muestro cómo usarla:

```Elixir
texto = "HOLA, MUNDO"
IO.puts String.downcase(texto)
```

La salida será:

```Elixir
"hola, mundo"
```

## Más Detalles

Elixir se vale de Unicode para la implementación de `String.downcase/1`, lo que significa que la función también funcionará con caracteres especiales o acentuados:

```Elixir
texto = "¡HÓLÁ, MŮŃDÓ!"
IO.puts String.downcase(texto)
```

La salida será:

```Elixir
"¡hólá, můňdó!"
```

Desde un punto de vista histórico, las funciones de normalización de texto, como convertir a minúsculas, han sido cruciales desde los primeros días del procesamiento de texto digital. Con la internacionalización de la informática, hacer esto correctamente para todo el espectro de caracteres Unicode se ha vuelto esencial.

Alternativamente, si necesitas intercaladamente mayúsculas y minúsculas en un texto, Elixir ofrece `String.swapcase/1`.

## Ver También

- [Elixir School: String Basics](https://elixirschool.com/es/lessons/basics/strings/)
- [String module documentation in Elixir official docs](https://hexdocs.pm/elixir/String.html)