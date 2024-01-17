---
title:                "Escribiendo pruebas"
html_title:           "Rust: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/writing-tests.md"
---

{{< edit_this_page >}}

# ¡Es hora de escribir pruebas en Rust!

## ¿Qué y por qué?

Escribir pruebas en Rust significa escribir código que verifica si nuestro programa funciona como debe. Los programadores hacen esto para asegurarse de que su código sea eficiente y libre de errores. De esta manera, podemos confiar en nuestro código y tener la tranquilidad de que funcionará correctamente.

## Cómo:

Para escribir pruebas en Rust, necesitamos utilizar la macro `assert!` que nos ayuda a comparar un valor esperado con el valor real. Si los dos valores son iguales, la prueba pasará; si son diferentes, la prueba fallará. Aquí hay un ejemplo:

```Rust
fn sum(x: i32, y: i32) -> i32 {
    x + y
}

#[test]
fn test_sum() {
    assert!(sum(2, 3) == 5);
}
```

## Profundizando

Escribir pruebas es una práctica común en la programación. Nos permite detectar errores en el código antes de que lleguen a producción y sean más costosos de arreglar. En Rust, también podemos usar la macro `assert_eq!` para verificar si dos valores son iguales y `assert_ne!` para verificar si son diferentes.

Hay otras herramientas disponibles para escribir pruebas en Rust, como `cargo test`, que nos permite ejecutar todas las pruebas de nuestro proyecto con un solo comando. También existen bibliotecas de pruebas como `rustfix` y `quickcheck` que pueden facilitar la escritura de pruebas más exhaustivas.

Además, escribir pruebas nos ayuda a documentar nuestro código y a comprender mejor su funcionalidad. Al indicar lo que se espera de nuestro programa en cada prueba, podemos tener una mejor comprensión de cómo debe funcionar y cómo interactúan sus diferentes partes.

## Ver también:

- [Documentación oficial de Rust sobre pruebas](https://doc.rust-lang.org/book/ch11-01-writing-tests.html)
- [Ejemplo de proyecto de Rust con pruebas](https://github.com/jonhoo/rust-fuzzing)
- [¿Por qué es importante escribir pruebas?](https://www.freecodecamp.org/news/why-testing-your-code-is-important/)

¡Ahora que conoces la importancia de escribir pruebas en Rust, asegúrate de integrar esta práctica en tu flujo de trabajo de programación! Con pruebas robustas, podrás confiar en tu código y reducir el tiempo de depuración en el futuro.