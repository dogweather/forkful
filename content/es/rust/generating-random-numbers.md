---
title:                "Rust: Generación de números aleatorios"
programming_language: "Rust"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué

Generar números aleatorios es una tarea común en la programación, especialmente en juegos, sorteos y simulaciones. En Rust, hay varias formas de generar números aleatorios que son eficientes y seguras de usar.

## Cómo hacerlo

Para generar números aleatorios en Rust, primero debemos importar el módulo ```rand``` y usar la función ```random``` para obtener un número aleatorio entre 0 y 1.

````Rust
use rand::prelude::*;

let random_num: f64 = random();
````

Si queremos un número aleatorio en un rango específico, podemos usar la función ```gen_range```, especificando el rango como argumentos.

````Rust
let random_range: i32 = rand::gen_range(1, 10);
````

También podemos generar números aleatorios dentro de una estructura de datos, como un vector o un array, usando la función ```shuffle```.

````Rust
let mut numbers = vec![1, 2, 3, 4, 5];
numbers.shuffle(&mut thread_rng()); // thread_rng() es un generador de números aleatorios seguro
````

## Profundizando

Detrás de estas funciones de generación de números aleatorios en Rust, hay algoritmos matemáticos complejos y seguros que garantizan que los números sean verdaderamente aleatorios. Además, el módulo ```rand``` también proporciona generadores de números aleatorios seguros para hilos, lo que significa que varios hilos pueden generar números aleatorios simultáneamente sin interferir entre sí.

## Ver también

- [Documentación oficial de Rust sobre generación de números aleatorios](https://doc.rust-lang.org/std/rand/)
- [Tutorial de Rust: Generación de números aleatorios](https://www.tutorialspoint.com/rust/rust_random_number.htm)
- [Ejemplo de uso de generación de números aleatorios en un juego en Rust](https://medium.com/@hydrostudios/rust-game-development-rng-d9c8290ad505)