---
title:                "Capitalizar una secuencia de caracteres"
html_title:           "Rust: Capitalizar una secuencia de caracteres"
simple_title:         "Capitalizar una secuencia de caracteres"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Si estás programando en Rust, es importante saber cómo capitalizar cadenas de texto ya que puede ser útil en diversas aplicaciones, como en la creación de nombres de usuario o en el formateo de datos para impresión.

## Cómo hacerlo

Para capitalizar una cadena de texto en Rust, primero debemos importar la biblioteca `std::string::String`. Luego, podemos utilizar el método `.make_ascii_uppercase()` para convertir la cadena a letras mayúsculas. Por ejemplo:

```Rust
use std::string::String;

fn main() {
    let texto = "hola mundo";
    let texto_mayus = String::from(texto).make_ascii_uppercase();
    println!("{}", texto_mayus); // Output: HOLA MUNDO
}
```

También podemos utilizar el método `.to_uppercase()` para capitalizar la primera letra de cada palabra en la cadena, en lugar de todas las letras. Por ejemplo:

```Rust
use std::string::String;

fn main() {
    let texto = "hola mundo";
    let texto_mayus = String::from(texto).to_uppercase();
    println!("{}", texto_mayus); // Output: Hola Mundo
}
```

## Profundizando

Cuando utilizamos el método `.make_ascii_uppercase()`, todas las letras de la cadena se convierten a mayúsculas, incluso las que tengan acentos o caracteres especiales. Esto se debe a que el método utiliza el conjunto de caracteres ASCII para realizar la conversión.

En cambio, al utilizar el método `.to_uppercase()`, se respeta la estructura de la cadena y solo se convierten las letras a mayúsculas según las reglas del idioma. Por ejemplo, en español la letra "ñ" se convierte a "Ñ" y no a "Ñ".

## Ver también

- Documentación oficial de Rust sobre cadenas de texto: https://doc.rust-lang.org/std/string/struct.String.html
- Tutorial de Rust en español: https://www.freecodecamp.org/news/como-aprender-rust-y-por-que-deberias-hacerlo/