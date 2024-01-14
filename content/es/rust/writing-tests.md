---
title:                "Rust: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué escribir pruebas en Rust
Escribir pruebas es una práctica clave en el desarrollo de software. Nos permite verificar que nuestro código funciona correctamente, detectar errores y mantener un alto nivel de calidad en nuestras aplicaciones. En Rust, escribir pruebas también nos ayudará a garantizar la seguridad y robustez de nuestro código.

## Cómo escribir pruebas en Rust
Para escribir pruebas en Rust, utilizamos el módulo `#[test]` y la macro `assert!()`. Veamos un ejemplo sencillo que prueba una función que suma dos números:

```Rust
#[test] // indica que esta es una prueba
fn test_suma() {
  let resultado = suma(2, 2); // llamamos a la función que queremos probar
  assert!(resultado == 4); // comprobamos que el resultado es igual a 4
}
```

Este es un ejemplo básico, pero podemos escribir pruebas más complejas y con diversos casos de prueba. También podemos utilizar la macro `assert_eq!()` para verificar que el resultado sea exactamente igual al esperado, y `assert_ne!()` para comprobar que sean diferentes.

## Profundizando en la escritura de pruebas
En Rust, además de las pruebas unitarias con el módulo `#[test]`, también podemos escribir pruebas de integración con el módulo `#[cfg(test)]`. Estas pruebas nos permiten probar nuestra aplicación como un todo, incluyendo la interacción entre los distintos módulos.

También es importante mencionar que podemos utilizar la herramienta `cargo test` para ejecutar todas las pruebas de nuestro proyecto de una sola vez. Además, en Rust tenemos la posibilidad de escribir pruebas para código concurrente y para manejo de errores de forma segura.

## Ver también
- [Documentación de pruebas en Rust](https://doc.rust-lang.org/book/ch11-01-writing-tests.html)
- [Tutorial de pruebas en Rust en español](https://dev.to/jdev6/como-escribir-pruebas-en-rust-4f8f)
- [Ejemplos de pruebas en Rust](https://github.com/jonhkr/rust-testing)