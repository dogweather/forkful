---
title:                "Extrayendo subcadenas"
html_title:           "Gleam: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Extraer subcadenas es una técnica que permite a los programadores manipular cadenas de texto para obtener una parte específica de ellas. Esta práctica es muy útil en situaciones en las que se necesita trabajar con datos específicos contenidos en una cadena más grande.

## Cómo:

```Gleam
let cadena = "Hola, mundo!"

let subcadena = String.substring(cadena, 0, 4)

**Salida:** Hola
```

En este ejemplo, utilizamos la función `substring` de la clase `String` para extraer los primeros cuatro caracteres de la cadena. También podemos utilizar `substring` para extraer una parte de una cadena desde un índice específico hasta el final de la cadena.

```Gleam
let cadena = "Hola, mundo!"

let subcadena = String.substring(cadena, 5, String.length(cadena))

**Salida:** mundo!
```

## Profundizando:

La extracción de subcadenas es una técnica muy común en la programación y ha estado presente en lenguajes de programación desde hace mucho tiempo. Algunos lenguajes como Java y JavaScript tienen métodos específicos para extraer subcadenas, mientras que en otros lenguajes se utilizan soluciones más complejas como expresiones regulares.

En Gleam, la función `substring` toma tres argumentos: la cadena de origen, el índice de inicio y el índice de fin de la subcadena. Esto le da al programador un mayor control sobre qué parte de la cadena se extrae.

## Ver también:

Si quieres aprender más sobre la extracción de subcadenas en Gleam, puedes consultar la documentación oficial en https://gleam.run/documentation/stdlib/string.html#substring. También puedes explorar otras funciones útiles en la clase `String` para trabajar con cadenas de texto en Gleam.