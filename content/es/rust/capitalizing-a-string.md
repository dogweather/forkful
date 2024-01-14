---
title:                "Rust: Capitalizando una cadena"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué usar Rust para capitalizar una cadena?

En la programación, a menudo nos encontramos con la necesidad de manipular cadenas de texto. Una de las tareas más comunes es capitalizar una cadena, es decir, convertir la primera letra de cada palabra en mayúscula.  Esto puede ser útil para crear títulos, nombres de archivos, o simplemente para hacer que una cadena sea más legible. En este artículo, aprenderemos cómo capitalizar una cadena de manera eficiente utilizando el lenguaje de programación Rust.

## Cómo hacerlo

Para capitalizar una cadena en Rust, utilizaremos el método `to_captialized()` de la estructura `String`. Este método toma la cadena y devuelve una nueva cadena con la primera letra de cada palabra en mayúscula. Veamos un ejemplo:

```Rust
let cadena = String::from("hola mundo");
let cadena_capitalizada = cadena.to_captialized();
println!("{}", cadena_capitalizada);
```
El resultado de este código sería "Hola Mundo". Como se puede ver, el método se encarga automáticamente de capitalizar la primera letra de cada palabra en la cadena.

## Profundizando

Además del método `to_captialized()`, Rust también proporciona el método `make_ascii_titlecase()` que capitaliza la cadena según las reglas del alfabeto ASCII. Esto significa que las letras acentuadas como "á" o "è" no serán capitalizadas. Si tu proyecto requiere una capitalización estricta basada en el alfabeto ASCII, este método sería la mejor opción. Sin embargo, en la mayoría de los casos, el método `to_captialized()` es suficiente.

Además de estos métodos, también podemos usar la macro `strict_capitalize!` del paquete crate `inflector` para capitalizar una cadena. Esta macro capitaliza la primera letra de cada palabra, pero también conserva el caso de las letras restantes. Por ejemplo, si tenemos una cadena "HELLO WORLD", la macro produciría "Hello World". Este enfoque puede ser útil si quieres conservar ciertos caracteres en mayúscula o minúscula.

## Ver también

- [Documentación oficial de Rust - Strings](https://doc.rust-lang.org/std/string/struct.String.html)
- [Crates.io - Inflector](https://crates.io/crates/inflector)
- [Introducing Strings in Rust](https://blog.logrocket.com/introducing-strings-in-rust/)