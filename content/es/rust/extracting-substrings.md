---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

# Extrayendo Subcadenas en Rust

## ¿Qué y Por qué?
Las subcadenas son fragmentos de una cadena de caracteres. Los programadores extraen subcadenas para manipular y analizar datos de manera más eficaz.

## Cómo se hace:
Extraer subcadenas en Rust es sencillo. Aquí tienes algunos ejemplos.

```Rust
 fn main() {
    let cadena = "Hola, Mundo";
    let subcadena = &cadena[0..4];
    println!("{}", subcadena); // Imprime "Hola"
}
```

En este código, hemos tomado la subcadena "Hola" en la cadena "Hola, Mundo". Los números entre corchetes indican el rango de la subcadena.

También puedes usar los métodos `chars()` y `nth()` para extraer caracteres de una cadena.

```Rust
fn main() {
    let cadena = "Hola, Mundo";
    let caracter = cadena.chars().nth(0).unwrap();
    println!("{}", caracter); // Imprime "H"
}
```

Aquí, hemos extraído el primer carácter, "H", de la cadena "Hola, Mundo".

## Inmersión profunda

La especificación de String en Rust no permite acceder directamente a los caracteres por su índice para garantizar la seguridad de la memoria. Rust es un lenguaje de programación que se preocupa profundamente por la seguridad y el rendimiento.

Además de la extracción de subcadenas, puedes dividir una cadena en áreas más pequeñas utilizando `split_whitespace()`, `split()`, `splitn()`.

```Rust
fn main() {
    let cadena = "Hola, Mundo";
    let palabras: Vec<&str> = cadena.split_whitespace().collect();
    println!("{:?}", palabras); // Imprime "["Hola,", "Mundo"]"
}
```

Este código divide la cadena "Hola, Mundo" en palabras.

En un entorno real, podrías encontrar otras bibliotecas útiles que te permiten manipular cadenas de manera más fácil y eficiente.

## Ver además

1. Documentación oficial de Rust: [https://doc.rust-lang.org/book/](https://doc.rust-lang.org/book/)
2. Un buen tutorial sobre las cadenas en Rust: [https://tourofrust.com/chapter_4_fr.html](https://tourofrust.com/chapter_4_fr.html)