---
title:                "Interpolación de cadenas de texto"
aliases:
- /es/rust/interpolating-a-string/
date:                  2024-01-20T17:51:36.864913-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolación de cadenas de texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qué & Por Qué?

La interpolación de cadenas nos permite infundir variables dentro de un texto, creando así una cadena personalizada en tiempo de ejecución. Los programadores la usan para construir mensajes dinámicos, combinando datos variables con texto estático de una manera legible y eficiente.

## Cómo hacerlo:

En Rust, interpolamos cadenas con la macros `format!`, `print!` o `println!`, donde las variables se incluyen dentro de llaves `{}`. Aquí hay un ejemplo simple:

```Rust
fn main() {
    let animal = "gato";
    let patas = 4;
    let mensaje = format!("El {} tiene {} patas.", animal, patas);
    println!("{}", mensaje);
}
```

Salida esperada:

```
El gato tiene 4 patas.
```

También puedes usar interpolación directamente con `println!`:

```Rust
fn main() {
    let comida = "empanadas";
    let cantidad = 12;
    println!("Hay {} {} en la mesa.", cantidad, comida);
}
```

Salida esperada:

```
Hay 12 empanadas en la mesa.
```

## Deep Dive

En otros lenguajes, como Python o Ruby, la interpolación de cadenas suele ser más directa, con sintaxis incorporada como `#{variable}` o `f"{variable}"`. Rust opta por una aproximación más explícita con sus macros para evitar errores en tiempo de compilación y mantener la seguridad en el manejo de memoria que caracteriza al lenguaje.

Otra alternativa en Rust es la concatenación de cadenas, pero es menos eficiente y más propensa a errores que la interpolación:

```Rust
fn main() {
    let adjetivo = "rápido";
    let objeto = "tren";
    let frase = "El ".to_owned() + adjetivo + " " + objeto + " va full.";
    println!("{}", frase);
}
```

Sin embargo, la interpolación de cadenas con `format!` o `println!` es la forma recomendada porque es más limpia y fácil de leer. Además, permite formateo avanzado, como especificar ancho, precisión y alineación.

## See Also

- Documentación oficial de Rust sobre `std::fmt`: [https://doc.rust-lang.org/stable/std/fmt/](https://doc.rust-lang.org/stable/std/fmt/)
- Rust by Example - Formatted print: [https://doc.rust-lang.org/rust-by-example/hello/print.html](https://doc.rust-lang.org/rust-by-example/hello/print.html)
- The Rust Programming Language - Display and Debug: [https://doc.rust-lang.org/book/ch05-02-example-structs.html#adding-useful-functionality-with-derived-traits](https://doc.rust-lang.org/book/ch05-02-example-structs.html#adding-useful-functionality-with-derived-traits)
