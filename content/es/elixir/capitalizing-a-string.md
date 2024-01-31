---
title:                "Capitalizando una cadena de texto"
date:                  2024-01-19
simple_title:         "Capitalizando una cadena de texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Capitalizar un string significa convertir la primera letra de una palabra a mayúscula. Los programadores lo hacen para asegurarse de que los nombres propios y títulos sigan las convenciones de escritura, o para mejorar la legibilidad de los textos.

## Cómo hacerlo:

En Elixir, puedes capitalizar strings usando la función `capitalize/1` de la librería `String`.

```elixir
original = "elixir es fantástico"
capitalizado = String.capitalize(original)
IO.puts capitalizado
```

Salida:

```
Elixir es fantástico
```

## Inmersión Profunda

La capitalización de strings es un concepto sencillo pero con muchas reglas lingüísticas si consideramos los distintos idiomas. En Elixir, la función `capitalize/1` pertenece al módulo `String`, que utiliza reglas de Unicode para manipular texto, lo cual es relevante debido a la diversidad de caracteres existente. Las alternativas incluyen el uso de expresiones regulares o el manejo manual de la posición de los caracteres para idiomas con reglas específicas que el módulo `String` no maneje por defecto.

Bajo el capó, Elixir se vale de la librería de Erlang para procesamiento de strings, la cual a su vez depende de la biblioteca ICU (International Components for Unicode), lo que asegura compatibilidad y manejo correcto de una amplia gama de caracteres y casuísticas.

## Ver También

- Documentación oficial de Elixir para el módulo `String`: https://hexdocs.pm/elixir/String.html
- Guía de inicio rápido de Elixir: https://elixir-lang.org/getting-started/introduction.html
- ICU Project para entender cómo se manejan strings a nivel internacional: http://site.icu-project.org/
