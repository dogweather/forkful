---
title:                "Interpolando una cadena de texto"
html_title:           "Haskell: Interpolando una cadena de texto"
simple_title:         "Interpolando una cadena de texto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

# La Interpolación de Cadenas en Rust: Una guía esencial

## ¿Qué y por qué?

Interpolar una cadena es el proceso de insertar variables dentro de una cadena. Los programadores lo hacen para maniobrar y presentar datos de manera más organizada y intuitiva.

## ¿Cómo hacerlo?

Puedes hacerlo de dos formas: directa y por formato.

**Directa:**

```Rust
let name = "Pablo";
let greeting = format!("Hola, {}", name);
println!("{}", greeting);   
```

**Salida:**

```
Hola, Pablo
```

**Por formato:**

```Rust
let name = "Pablo";
println!("Hola, {}", name);
```

**Salida:**

```
Hola, Pablo
```

## Análisis en profundidad

Interpolar cadenas ha sido una característica predilecta en lenguajes de programación por su practicidad. Rust adopta una aproximación más segura, evitando concatenar cadenas de manera directa y favoreciendo el uso de `format!()` y `println!()`.

Podrías concatenar cadenas manualmente, pero no es recomendable. La eficiencia y la claridad del código pueden sufrir, además de introducir posibles errores.

Rust trata las cadenas de forma diferente a otros lenguajes. Las cadenas son tipos de datos que se almacenan en la memoria de "heap", y son inmutables.

## Ver también

- Manual de Rust para formatos en cadena: https://doc.rust-lang.org/std/fmt/
- Discusión en profundidad sobre cadenas en Rust:  https://www.ameyalokare.com/rust/2017/10/12/rust-str-vs-String.html
- Documentación oficial de Rust:  https://doc.rust-lang.org/book/