---
title:                "Rust: Convirtiendo una cadena en minúsculas"
simple_title:         "Convirtiendo una cadena en minúsculas"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

Convertir cadenas de texto a minúsculas es una tarea común en muchos programas de software. Puede ser útil para comparar cadenas de manera más precisa o para formatear correctamente la entrada del usuario. En Rust, hay varias formas de lograr esto, dependiendo de lo que esté buscando específicamente.

## Cómo hacerlo

Hay varias formas de convertir una cadena a minúsculas en Rust. Una forma sencilla es utilizar el método `to_lowercase` de la estructura `String`. Por ejemplo:

```rust 
let my_string = String::from("HOLA MUNDO");
let lowercase_string = my_string.to_lowercase();

println!("{}", lowercase_string);
```

Esto imprimirá "hola mundo". También puede utilizar el método `to_lowercase` en un literal de cadena, en lugar de crear una instancia de `String`.

Además, si está trabajando con cadenas que contienen caracteres no ASCII, puede utilizar `to_lowercase` de `Unicode` para asegurarse de que se manejen correctamente las mayúsculas y minúsculas, utilizando la función `fold` para iterar sobre los caracteres de la cadena.

```rust
let my_string = String::from("ÆRLIGE INNGANG");
let lowercase_string = my_string.chars()
    .fold(String::new(), |mut acc, c| {
        acc.extend(c.to_lowercase());
        acc
});

println!("{}", lowercase_string);
```

Esto devolverá "ærlige inngang".

## Profundizando

Cuando se trata de convertir cadenas a minúsculas en Rust, es importante entender cómo funciona el proceso detrás de escena. En primer lugar, Rust utiliza el estándar Unicode para manejar los caracteres, lo que significa que incluso los caracteres no ASCII se manejan correctamente.

Además, el método `to_lowercase` utiliza la tabla de casos Unicode, lo que significa que cada carácter se mapea a su equivalente en minúsculas en función de sus propiedades de caso.

Una cosa importante a tener en cuenta es que el método `to_lowercase` devuelve una nueva instancia de `String` en lugar de modificar la cadena original. Esto es porque las cadenas en Rust son inmutables por defecto, lo que significa que no pueden ser modificadas después de su creación. Si desea modificar la cadena original, puede utilizar el método `make_ascii_lowercase`, que solo funciona con caracteres ASCII.

## Ver también 

- [Documentación oficial de Rust sobre cadenas y methods](https://doc.rust-lang.org/std/string/struct.String.html)
- [Ejemplos de conversión de cadenas en el libro de Rust](https://doc.rust-lang.org/book/ch08-02-strings.html#creating-a-new-string-from-a-string-slice)
- [Unicode en Rust](https://blog.burntsushi.net/ripgrep/unicode/)