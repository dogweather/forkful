---
title:                "Rust: Escribiendo pruebas"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué escribir pruebas en Rust

Escribir pruebas es una práctica esencial en el desarrollo de software en cualquier lenguaje, y Rust no es la excepción. Aunque pueda parecer tedioso o innecesario, las pruebas juegan un papel crucial en la garantía de la calidad del código y en la detección temprana de posibles errores.

## Cómo escribir pruebas en Rust

Escribir pruebas en Rust es relativamente sencillo gracias a su sistema de pruebas integrado. A continuación, se presenta un ejemplo de una función que calcula el área de un triángulo y su respectiva prueba:

```rust
// Función que calcula el área de un triángulo
fn area_triangulo(base: f32, altura: f32) -> f32 {
    (base * altura) / 2.0
}

// Prueba de la función area_triangulo
#[test]
fn test_area_triangulo() {
    assert_eq!(area_triangulo(5.0, 10.0), 25.0);
}
```

En este ejemplo, se definió una función `area_triangulo` que recibe la base y altura del triángulo como parámetros y retorna su área. Luego, se escribió una prueba utilizando el atributo `#[test]`, que indica que estamos escribiendo una prueba. Dentro de la prueba, utilizamos la macro `assert_eq`, que compara el resultado de la función con el valor esperado (en este caso, 25.0).

Para correr las pruebas, solo necesitamos ejecutar el siguiente comando en la terminal:

```sh
cargo test
```

Esto ejecutará todas las pruebas del proyecto y nos mostrará el resultado de cada una. Si alguna prueba falla, nos indicará el lugar exacto donde ocurrió el error.

## Deep Dive

Aparte de las pruebas unitarias, en Rust también podemos escribir pruebas de integración, que verifican la interacción entre varios componentes de nuestro código. Además, Rust también cuenta con herramientas y bibliotecas para realizar pruebas de rendimiento y pruebas de propiedad.

Es importante destacar que escribir pruebas no reemplaza la necesidad de realizar pruebas manuales, pero sí ayuda a encontrar errores de manera más temprana y a tener una mayor confianza en nuestro código.

## Ver también

- [Documentación de Rust sobre pruebas](https://doc.rust-lang.org/book/ch11-01-writing-tests.html)
- [Ejemplos de pruebas en el repositorio oficial de Rust](https://github.com/rust-lang/rust/tree/master/src/test/ui)
- [Biblioteca de pruebas `assert`](https://doc.rust-lang.org/std/macro.assert.html)
- [Biblioteca de pruebas de propiedad `quickcheck`](https://github.com/BurntSushi/quickcheck)

Gracias por leer este post y espero que te sea útil en tu aprendizaje de Rust. ¡Hasta la próxima!