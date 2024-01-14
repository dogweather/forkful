---
title:    "Rust: Escribiendo pruebas"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por qué escribir pruebas en Rust

Cuando se trata de programación, escribir pruebas puede parecer una tarea tediosa y adicional. Sin embargo, es una práctica esencial para garantizar la calidad y la estabilidad de nuestro código. Al escribir pruebas en Rust, podemos detectar errores y fallos de manera temprana, lo que nos ayuda a ahorrar tiempo y esfuerzo en el futuro.

## Cómo escribir pruebas en Rust

Para escribir pruebas en Rust, debemos seguir los siguientes pasos:

1. Importar el módulo `test` de Rust.
```Rust
use test::Bencher;
```

2. Agregar el atributo `#[test]` encima de la función de prueba.
```Rust
#[test]
fn suma_correcta() {
    let resultado = 2 + 2;
    assert_eq!(resultado, 4);
}
```

3. Ejecutar las pruebas con el comando `cargo test`.
```
$ cargo test
```

4. Verificar la salida de las pruebas en la consola.
```
running 1 test
test suma_correcta ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

## Profundizando en la escritura de pruebas

Escribir pruebas en Rust no solo nos ayuda a detectar errores, sino que también nos permite documentar nuestro código y tener una mejor comprensión de su funcionamiento. Al escribir pruebas, estamos obligados a pensar en diferentes casos de uso y a asegurarnos de que nuestro código sea robusto.

Además, Rust ofrece herramientas como `assert_eq!()` y `assert_ne!()` que nos permiten evaluar la igualdad y la desigualdad entre valores. También podemos usar la macro `#[should_panic]` para probar si una función falla correctamente cuando se le pasan argumentos inválidos.

En resumen, escribir pruebas en Rust nos ayuda a desarrollar un código más confiable, legible y fácil de mantener.

## Ver también

- [Documentación oficial de Rust sobre pruebas](https://doc.rust-lang.org/book/ch11-01-writing-tests.html)
- [Guía práctica de test-driven development en Rust](https://blog.thoughtram.io/announcements/pragmatic-tdd-with-rust.html)
- [Ejemplo de proyecto de Rust con pruebas incluidas](https://github.com/exercism/rust)