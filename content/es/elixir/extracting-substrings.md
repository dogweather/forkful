---
title:                "Extrayendo subcadenas"
html_title:           "Elixir: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qué y Por qué
Extraer subcadenas es una técnica muy útil en programación que consiste en obtener una parte específica de una cadena más grande. Los programadores lo hacen para manipular y trabajar con datos más específicos, permitiendo un mejor control y manejo de la información.

## Cómo:
```Elixir
cadena = "Este es un ejemplo de cadena"
extraido = String.slice(cadena, 5..8)
Enum.each(extraido, fn x ->
    IO.puts(x)
end)

# Output: e s u
```

## Profundizando
Extraer subcadenas se ha utilizado desde los primeros días de la programación, cuando se trabajaba principalmente con lenguajes como C y Fortran. En lugar de extraer subcadenas, los programadores a menudo optaban por manipular directamente la memoria del sistema para obtener los datos deseados. Sin embargo, gracias a la evolución de los lenguajes de programación y la inclusión de bibliotecas estándar, ahora es mucho más fácil y seguro extraer subcadenas en Elixir.

Además de la función `String.slice` mostrada en el ejemplo anterior, también existen otras alternativas para extraer subcadenas, como el método `String.split` que permite dividir una cadena en varias partes según un delimitador específico.

## Ver también
- [Documentación oficial de Elixir sobre la función String.slice](https://hexdocs.pm/elixir/String.html#slice/2)
- [Método String.split en Elixir](https://hexdocs.pm/elixir/String.html#split/2)