---
title:                "Buscando y reemplazando texto"
html_title:           "Rust: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Reemplazar texto es una tarea común en la programación, en la que se busca un fragmento de texto específico y se reemplaza por otro. Los programadores lo hacen para corregir errores, hacer cambios masivos de texto, o para mejorar la eficiencia de su código.

## Cómo:

Aquí hay un ejemplo simple de cómo buscar y reemplazar texto en Rust:

```Rust
let texto = "Hola! Me llamo Rust.";
let nuevo_texto = texto.replace("Rust", "Ferrugem"); // Reemplaza "Rust" con "Ferrugem"
println!("{}", nuevo_texto); // Imprime "Hola! Me llamo Ferrugem."
```

## Profundizando:

En el pasado, la búsqueda y reemplazo de texto se hacía manualmente, lo que requería mucho tiempo y esfuerzo. Ahora, gracias a herramientas como Rust, esta tarea se puede realizar de manera más eficiente. Además de la función `replace()` mostrada anteriormente, hay otras formas de buscar y reemplazar texto en Rust, como el uso de expresiones regulares.

## Ver también:

- [Documentación oficial de Rust sobre la función replace()](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Tutorial de Rust sobre expresiones regulares](https://doc.rust-lang.org/book/ch09-06-pattern-syntax.html#regular-expressions)