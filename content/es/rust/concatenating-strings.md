---
title:                "Concatenando cadenas"
html_title:           "Rust: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La concatenación de cadenas es el proceso de unir dos o más cadenas de texto en una sola. Los programadores lo hacen para crear cadenas más largas y complejas a partir de piezas más pequeñas, lo que los ayuda a crear una salida de texto más dinámica y personalizada.

## ¿Cómo hacerlo?

```Rust
let nombre = "Juan";
let apellido = "Pérez";

let nombre_completo = format!("{} {}", nombre, apellido);
println!("Hola, mi nombre es {}", nombre_completo);
```
**Salida:**
```
Hola, mi nombre es Juan Pérez
```

En el ejemplo de código anterior, se utiliza el método `format!` para concatenar las variables `nombre` y `apellido` en la variable `nombre_completo`. Luego, se imprime un saludo personalizado utilizando la cadena concatenada.

## Profundizando

La concatenación de cadenas es una técnica comúnmente utilizada en la programación, especialmente en la manipulación de texto. En Rust, también se puede realizar concatenación utilizando el operador `+` entre dos cadenas, sin embargo, esto crea una nueva cadena con un rendimiento ligeramente inferior que el método `format!`.

Para hacer una concatenación de forma más eficiente, se pueden usar los macro-funciones `String::push_str()` o `String::push()` para agregar contenido a una cadena existente. Además, Rust ofrece el tipo de datos `String` que es mutable y permite la modificación de su contenido, lo que resulta útil para la concatenación de cadenas.

## Ver también

Para obtener más información sobre cómo manejar cadenas en Rust, consulta la [documentación oficial](https://doc.rust-lang.org/std/string/struct.String.html). También puedes explorar las [alternativas](https://www.techiedelight.com/concatenation-rust-language/) a la concatenación de cadenas en Rust.