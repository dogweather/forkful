---
title:                "Utilizando expresiones regulares"
html_title:           "Gleam: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

¡Hola programadores de Gleam!

¿Alguna vez has tratado de buscar o manipular cadenas de texto en tus programas y has terminado con un código confuso y lleno de líneas innecesarias? Bueno, tengo una solución para ti: ¡las expresiones regulares en Gleam!

## ¿Qué y por qué?

Las expresiones regulares son patrones de texto que nos permiten buscar y manipular cadenas de caracteres con una sintaxis concisa y poderosa. Son utilizadas por programadores para simplificar y optimizar la manipulación de cadenas de texto, lo que resulta en un código más legible y eficiente.

## ¡Cómo hacerlo!

A continuación, te mostraré un par de ejemplos sencillos de cómo utilizar expresiones regulares en Gleam para que puedas empezar a aprovechar su potencial.

```Gleam
// Encontrar una dirección de correo electrónico en una cadena de texto
let regex = Regex.new(~r/\w+@\w+\.\w+/)
let text = "Mi correo electrónico es ejemplo@ejemplo.com"
match regex.search(text) {
  Some((match_text, _)) -> match_text
  None -> "No se encontró una dirección de correo electrónico"
}
```

```Gleam
// Reemplazar una palabra en una cadena de texto
let regex = Regex.new(~r/hola/)
let text = "¡Hola mundo!"
text
|> regex.replace(~r/mundo/, "a ti")
|> assert Ok("¡Hola a ti!")
```

## Deep Dive

Las expresiones regulares existen desde la década de 1950 y han sido ampliamente adoptadas en diferentes lenguajes de programación. Sin embargo, hay otras formas de manejar cadenas de texto, como las funciones de cadena incorporadas en Gleam o el uso de la librería Stringr. Muchos programadores prefieren utilizar expresiones regulares debido a su flexibilidad y su capacidad de hacer búsquedas y manipulaciones más complejas.

En Gleam, las expresiones regulares son implementadas utilizando la librería PCRE (Perl Compatible Regular Expressions), lo que significa que las sintaxis y los patrones son similares a los de otros lenguajes de programación. Además, Gleam ofrece funciones útiles para trabajar con expresiones regulares, como ```search```, ```replace``` y ```matches```.

## Ver También

- Documentación de expresiones regulares en la Wiki de Gleam: https://github.com/gleam-lang/gleam/wiki/Documentation-Regexp
- Una guía práctica sobre el uso de expresiones regulares en Gleam: https://dev.to/gleam_lang/basics-of-regular-expressions-in-gleam-ljn