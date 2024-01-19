---
title:                "Usando expresiones regulares"
html_title:           "Go: Usando expresiones regulares"
simple_title:         "Usando expresiones regulares"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Las expresiones regulares son patrones utilizados para encontrar coincidencias de caracteres en el texto. Los programadores las usan para ahorrar tiempo y mejorar la eficiencia de la manipulación de texto.

## ¿Cómo?
Aquí hay algunos ejemplos de cómo usar expresiones regulares en Gleam.

```Gleam
import gleam/regex

let re = regex.from_string("[0-9]+")

case re {
  Ok(re) ->
    regex.find(re, "Hello, I am 28 years old and my phone number is 123456789")
  Error(_) ->
    "Invalid regex"
}
```
Output:

```Gleam
Ok([#Ok("28"), #Ok("123456789")])
```
## Inmersión Profunda
En cuanto al contexto histórico, las expresiones regulares se popularizaron con las primeras versiones de Perl en los años 80. Pero en estos días, casi todos los lenguajes de programación las admiten, incluido Gleam.

En términos de alternativas, en algunos casos podrías usar métodos de cadena integrados (como `to_string()` o `split()`), pero las expresiones regulares suelen ser más versátiles.

Las expresiones regulares en Gleam están implementadas a través del uso de las bibliotecas de Rust subyacentes, lo que garantiza un rendimiento y seguridad potentes.

## Ver También
1. [Documentación de Gleam/Regex](https://hexdocs.pm/gleam_stdlib/gleam/regex/)
2. [Manual de usuario regex](https://www.regular-expressions.info/tutorial.html)
3. [Practicar regex online](https://regex101.com/)