---
title:                "Extrayendo subcadenas"
html_title:           "Elm: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

##¿Por qué extraer subcadenas?

Extraer subcadenas es una habilidad importante en la programación ya que te permite manipular y obtener información específica de una cadena de texto. Esto puede ser útil en una variedad de situaciones, como en la creación de filtros de búsqueda o en la validación de datos de usuario. Aprender cómo extraer subcadenas te ayudará a ampliar tus capacidades como programador en Elm.

##Cómo hacerlo

La función `String.slice` se utiliza para extraer subcadenas de una cadena existente en Elm. Toma tres argumentos: la cadena de origen, el índice inicial y el índice final. Por ejemplo:

```Elm
cadena = "Hola Mundo"
subcadena = String.slice cadena 0 4
-- Esto extraerá la subcadena "Hola"
```

También puedes utilizar índices negativos para contar desde el final de la cadena, por ejemplo:

```Elm
cadena = "Hola Mundo"
subcadena = String.slice cadena (-4) (-1)
-- Esto extraerá la subcadena "ndo"
```

Si solo se proporciona el índice inicial, se extraerá una subcadena desde ese índice hasta el final de la cadena:

```Elm
cadena = "Hola Mundo"
subcadena = String.slice cadena 4
-- Esto extraerá la subcadena "Mundo"
```

Recuerda que los índices en Elm comienzan en 0, por lo que el primer carácter en una cadena tiene un índice de 0.

##Profundizando

La función `String.slice` en realidad utiliza dos índices para determinar la subcadena: el índice inicial inclusivo y el índice final no inclusivo. Esto significa que el carácter en el índice final no se incluirá en la subcadena resultante.

También es posible utilizar valores de punto flotante para los índices, lo que te permite extraer subcadenas específicas dentro de una cadena. Por ejemplo:

```Elm
cadena = "Mi nombre es Juan"
subcadena = String.slice cadena 8 12
-- Esto extraerá la subcadena "es"
```

Además, si el índice inicial es mayor que el índice final, la función `String.slice` devolverá una cadena vacía. Esto puede ser útil para validar los datos de entrada antes de intentar extraer una subcadena.

##Ver también

- [Documentación oficial de Elm sobre la función `String.slice`](https://package.elm-lang.org/packages/elm/core/latest/String#slice)
- [Tutoriales de Elm en español](https://www.elm-tutorial.org/es/)