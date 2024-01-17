---
title:                "Extrayendo subcadenas"
html_title:           "Rust: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Extraer subcadenas se refiere a obtener una parte específica de una cadena de texto. Los programadores a menudo hacen esto para realizar operaciones en una sección específica de una cadena, como buscar una palabra clave o formatear una fecha.

## Cómo hacerlo:
La biblioteca estándar de Rust proporciona varias formas de extraer subcadenas, incluyendo el método `substring()` de la clase `String` y la función `slice()` de la clase `str`. A continuación se muestra un ejemplo de ambas opciones y su salida correspondiente en la consola:

```Rust
// Ejemplo usando substring()
let cadena = "Hola Mundo";
let subcadena = cadena.substring(5, 10); // obtiene "Mundo"
println!("{}", subcadena); // imprime "Mundo"

// Ejemplo usando slice()
let cadena = "Hola Mundo";
let subcadena = &cadena[5..10]; // obtiene "Mundo"
println!("{}", subcadena); // imprime "Mundo"
```

## Profundizando:
La extracción de subcadenas ha existido desde los primeros lenguajes de programación, pero ha evolucionado mucho a lo largo de los años. Antes, los programadores tenían que realizar cálculos manuales para determinar los índices de inicio y fin de una subcadena dentro de una cadena. Sin embargo, con el avance de las bibliotecas y funciones de manejo de cadenas, ahora es mucho más fácil y eficiente extraer subcadenas.

En términos de alternativas, los programadores también pueden utilizar expresiones regulares para extraer subcadenas en Rust. Sin embargo, esto puede ser más complejo y requiere un mayor conocimiento de las expresiones regulares.

Internamente, Rust utiliza punteros inteligentes para almacenar y manipular cadenas de forma eficiente. Además, los métodos `substring()` y `slice()` utilizan el tipo de dato `Index` de Rust, que permite el acceso a partes específicas de una cadena utilizando la sintaxis de índice. Esto hace que la extracción de subcadenas en Rust sea muy eficiente y fácil de implementar.

## Ver también:
Para aprender más sobre la extracción de subcadenas en Rust, puedes consultar la documentación oficial de Rust en https://doc.rust-lang.org/std/string/struct.String.html#method.substring y https://doc.rust-lang.org/std/primitive.str.html#method.slice. También puedes explorar la biblioteca regex de Rust para ver cómo utilizar expresiones regulares para extraer subcadenas.