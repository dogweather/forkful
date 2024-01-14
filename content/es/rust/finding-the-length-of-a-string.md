---
title:                "Rust: Encontrando la longitud de una cadena."
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, a menudo necesitamos encontrar la longitud de una cadena de texto para realizar ciertas operaciones. En este artículo, aprenderemos cómo hacerlo en Rust de manera eficiente.

## Cómo hacerlo

Para encontrar la longitud de una cadena de texto en Rust, podemos usar el método `len()` que está disponible en todos los tipos de datos `String` y `&str`. Aquí hay un ejemplo de cómo usarlo:

```Rust
let mi_cadena = "¡Hola, mundo!";
let longitud = mi_cadena.len();

println!("La longitud de la cadena es: {}", longitud);
```

Este código imprimirá: `La longitud de la cadena es: 13`.

También podemos usar `len()` en una cadena de texto que almacenamos en una variable de tipo `String`:

```Rust
let mi_cadena = String::from("¡Hola, mundo!");
let longitud = mi_cadena.len();

println!("La longitud de la cadena es: {}", longitud);
```

De esta manera, también obtendremos una longitud de `13`.

## Profundizando

Hay una diferencia técnica entre usar `len()` en una cadena de texto almacenada en una variable de tipo `String` y en una cadena de texto literal. Al usarlo en una variable de tipo `String`, se llama al método `len()` del tipo `String`, mientras que en una cadena de texto literal, se llama al método `len()` del tipo `&str`.

Además, también podemos utilizar el método `chars()` en una cadena de texto para obtener un iterador que nos permite recorrer cada carácter de la cadena. Luego, podemos usar el método `count()` en este iterador para obtener la cantidad total de caracteres en la cadena.

```Rust
let mi_cadena = "¡Hola, mundo!";
let cantidad_caracteres = mi_cadena.chars().count();

println!("La cantidad de caracteres en la cadena es: {}", cantidad_caracteres);
```

Este código imprimirá: `La cantidad de caracteres en la cadena es: 13`.

## Véase también

- [La documentación oficial de Rust sobre la manipulación de cadenas de texto](https://doc.rust-lang.org/std/string/index.html)
- [El tutorial de Rust Strings de Rust by Example](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)