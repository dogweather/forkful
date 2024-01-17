---
title:                "Imprimiendo salida de depuración"
html_title:           "Rust: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Imprimir información de depuración (debug output) es una técnica común en la programación para mostrar mensajes, valores de variables y otros datos relevantes durante el proceso de desarrollo y depuración de código. Los programadores lo hacen para entender mejor el flujo del programa y detectar errores más fácilmente.

## Cómo:

```Rust
// Crear una variable y mostrar su valor en la consola
let num = 10;
println!("El valor de num es: {}", num);

// Mostrar un mensaje en la consola
println!("¡Hola, mundo!");

// Mostrar información de una estructura
struct Persona {
    nombre: String,
    edad: u8,
}

let persona = Persona {
    nombre: String::from("Juan"),
    edad: 25,
};

println!("La persona se llama {} y tiene {} años.", persona.nombre, persona.edad);
```

## Inmersión profunda:

Imprimir información de depuración ha sido una práctica común desde los primeros días de la programación, ya que ayuda a los programadores a entender cómo funciona el código y a encontrar problemas. Además de `println!`, otra forma de imprimir información de depuración en Rust es utilizando la macro `dbg!`, que es útil cuando se necesita imprimir datos de forma más detallada o en un formato específico.

También existen otras herramientas y métodos para imprimir información de depuración en Rust, como las trazas de ejecución (logging) y las herramientas de depuración integradas en la mayoría de los entornos de desarrollo.

En términos de implementación, las macros `println!` y `dbg!` utilizan el mismo mecanismo interno en Rust para imprimir información en la consola. Este mecanismo se basa en la capacidad de Rust de formatear cadenas de texto en tiempo de compilación, lo que lo hace muy eficiente en comparación con otros lenguajes de programación.

## Ver también:

- [Documentación oficial de Rust sobre macros de depuración](https://doc.rust-lang.org/std/macro.dbg.html)
- [Guía práctica sobre la impresión de información de depuración en Rust](https://dev.to/rust-avengers/the-complete-guide-to-debugging-in-rust-2ejp)