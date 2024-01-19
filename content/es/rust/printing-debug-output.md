---
title:                "Imprimiendo salida de depuración"
html_title:           "Arduino: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

# Entendiendo la impresión de la salida de depuración en Rust

## ¿Qué y por qué?

La impresión de la salida de depuración es una forma de rastrear y diagnosticar el estado de tu programa durante su ejecución. Los programadores lo hacen para descubrir y resolver errores.

## Cómo hacerlo:

```Rust
# Mostrando un entero de forma depurada
let x = 5;
println!("x = {:?}", x);
```

La salida será: `x = 5`

Para estructuras de datos más complejas:

```Rust
# Una tupla
let x = (32, "Hola");
println!("{:?}", x);

# Un vector
let x = vec![1, 2, 3];
println!("{:?}", x);
```

Las salidas serán: `(32, "Hola")` y `[1, 2, 3]` respectivamente.

## Buceo Profundo

1. **Contexto histórico:** Rust, lanzado en 2010, tomó la noción de salida de depuración de lenguajes antiguos como C++, incorporando mejoras en legibilidad y manejo de errores.
2. **Alternativas:** La biblioteca `log` de Rust proporciona capacidades de registro más avanzadas si necesitas más control que el ofrecido por `println!`.
3. **Detalles de implementación:** `println!("{:?}", x)` utiliza el `trait` `Debug` del lenguaje para formatear la salida. `Debug` está diseñado para la depuración del programador, con formato libre y diseñado para ser humano-legible.

## Ver También

1. [Documentación oficial de Rust](https://doc.rust-lang.org/book/ch18-03-pattern-syntax.html)
2. [El Libro de Rust](https://doc.rust-lang.org/book/ch18-02-refutability.html)
3. [Guía de Rust](https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html)