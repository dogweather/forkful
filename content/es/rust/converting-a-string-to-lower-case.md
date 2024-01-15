---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Rust: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una cadena a minúsculas en Rust?

Convertir una cadena a minúsculas es útil cuando se desea comparar cadenas de manera más precisa o cuando se quiere asegurar que ciertos datos estén en un formato uniforme. En Rust, hay varias formas de lograr esto y en este artículo te mostraremos cómo hacerlo.

## Cómo hacerlo

El método más común para convertir una cadena a minúsculas en Rust es utilizando la función `to_lowercase()` del tipo de datos `String`. Este método devuelve una nueva cadena con todas las letras en minúsculas. Veamos un ejemplo:

```Rust
let cadena = String::from("HOLA MUNDO");
let cadena_min = cadena.to_lowercase();

println!("{}", cadena_min); // imprimirá "hola mundo"
```

También es posible utilizar el método `to_lowercase()` en una cadena literal utilizando el operador `&`:

```Rust
let cadena = "HOLA MUNDO";
let cadena_min = cadena.to_lowercase();

println!("{}", cadena_min); // imprimirá "hola mundo"
```

Si se tiene una cadena que contiene caracteres no primarios, como letras con acentos o símbolos, también es posible convertirlos a minúsculas utilizando `to_lowercase()`.

```Rust
let cadena = "ÁRBOL";
let cadena_min = cadena.to_lowercase();

println!("{}", cadena_min); // imprimirá "árbol"
```

Es importante tener en cuenta que `to_lowercase()` devuelve una nueva cadena y no modifica la original. 

Otra forma de convertir a minúsculas es utilizando el método `to_ascii_lowercase()`. Este método también convierte caracteres a minúsculas, pero solo toma en cuenta los caracteres ASCII:

```Rust
let cadena = "¡HOLA MUNDO!";
let cadena_min = cadena.to_ascii_lowercase();

println!("{}", cadena_min); // imprimirá "¡hola mundo!"
```

Por último, si se desea manipular la cadena original y no crear una nueva, también es posible utilizar el método `make_ascii_lowercase()`:

```Rust
let mut cadena = String::from("HOLA MUNDO");
cadena.make_ascii_lowercase();

println!("{}", cadena); // imprimirá "hola mundo"
```

## Explorando más a fondo

Internamente, tanto `to_lowercase()` como `make_ascii_lowercase()` utilizan el módulo `std::ascii`, mientras que `to_ascii_lowercase()` utiliza el módulo `std::ascii::AsciiExt`. Estos módulos contienen funciones de conversión de cadenas a ASCII y viceversa.

También es posible convertir solo la primera letra de una cadena a minúscula utilizando el método `to_lowercase()` con el método `chars()` para obtener un iterador de caracteres. Podemos luego utilizar el método `next()` para obtener solo el primer carácter y luego volver a convertirlo a minúsculas utilizando `to_lowercase()`:

```Rust
let mut cadena = String::from("HOLA");
let first_char = cadena.chars().next().expect("La cadena está vacía!");
cadena.replace_range(..1, &first_char.to_lowercase().to_string());

println!("{}", cadena); // imprimirá "hOLA"
```

## Ver también

- [Documentación oficial sobre cadenas en Rust](https://doc.rust-lang.org/std/string/struct.String.html)
- [Módulo `std::ascii`](https://doc.rust-lang.org/std/ascii/)

Esperamos que este artículo te haya sido útil en tus proyectos con Rust. ¡Feliz hacking!