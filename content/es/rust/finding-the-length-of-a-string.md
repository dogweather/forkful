---
title:                "Encontrando la longitud de una cadena"
html_title:           "Rust: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué es importante encontrar la longitud de una cadena en Rust?

En la programación, a menudo necesitamos manipular cadenas de texto para realizar ciertas tareas. Una de estas tareas es encontrar la longitud de una cadena, es decir, la cantidad de caracteres que contiene. Esto puede ser útil para realizar operaciones posteriores en la cadena, como separarla en diferentes partes o realizar búsquedas en ella.

## Cómo encontrar la longitud de una cadena en Rust

Para encontrar la longitud de una cadena en Rust, tenemos que utilizar el método `.len()` en el objeto de tipo `String`. Este método nos devolverá un valor de tipo `usize`, que representa la cantidad de caracteres en la cadena.

```
Rust
let cadena = String::from("¡Hola, mundo!");
let longitud = cadena.len();

println!("La longitud de la cadena es: {}", longitud);
```

Este código imprimirá "La longitud de la cadena es: 13", ya que nuestra cadena tiene 13 caracteres.

Si queremos usar una cadena que no sea variable (un string literal), también podemos utilizar el método `.len()` directamente en la cadena:

```
Rust
let cadena = "¡Hola, mundo!";
let longitud = cadena.len();

println!("La longitud de la cadena es: {}", longitud);
```

Este código tendrá el mismo resultado que el anterior.

## Profundizando en la búsqueda de la longitud de una cadena

Cuando utilizamos el método `.len()` en una cadena, en realidad estamos utilizando una referencia a esa cadena, ya que las cadenas en Rust son representadas como una combinación de un puntero a la memoria que contiene los caracteres y un tamaño. Esto se debe a que Rust no permite tener valores desconocidos o nulos.

También es importante tener en cuenta que el método `.len()` cuenta la cantidad de bytes en una cadena, lo que puede no ser igual a la cantidad de caracteres dependiendo de la codificación utilizada (como UTF-8). Para este caso, es posible usar el método `.chars().count()` para contar la cantidad de caracteres en la cadena.

## Ver también

- [La documentación oficial de Rust para el método len()](https://doc.rust-lang.org/std/string/struct.String.html#method.len)
- [The Rust Programming Language book, Chapter 8: Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)