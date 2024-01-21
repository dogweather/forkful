---
title:                "Calculando la longitud de una cadena"
date:                  2024-01-20T17:47:23.522063-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calculando la longitud de una cadena"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Encontrar la longitud de un string significa saber cuántos caracteres contiene. Los programadores lo hacen para validar entradas, recorrer strings o para operaciones de formateo.

## Cómo hacerlo:
```gleam
fn main() {
  let saludo = "¡Hola, Mundo!"
  let longitud = string.length(saludo)
  io.println(longitud) // Imprime: 13
}
```
El código usa la función `string.length`, que te da el número exacto de caracteres del string.

## Profundización
Antes, en otros lenguajes, medir strings podía ser complicado por cómo almacenaban los caracteres (piensa en C con los terminadores nulos). En Gleam, como en muchos lenguajes modernos, contar la longitud de un string es directo y maneja Unicode correctamente, evitando malentendidos con caracteres que puedan ocupar más de un byte.

Alternativas a `string.length` podrían incluir funciones que recorren el string y cuentan caracteres, pero aparte de ser innecesario en Gleam, también serían menos eficientes.

En cuanto a la implementación, la función `string.length` tipicamente cuenta los "code points" en el string, que es una representación más acertada para textos Unicode.

## Ver También
- Documentación oficial de Gleam en strings: [Gleam String Docs](https://gleam.run/book/tour/strings.html)
- Guía Unicode para entender caracteres en strings: [Unicode String Guide](http://unicode.org/standard/principles.html)