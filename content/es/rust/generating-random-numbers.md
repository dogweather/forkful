---
title:                "Generación de números aleatorios"
html_title:           "Rust: Generación de números aleatorios"
simple_title:         "Generación de números aleatorios"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Por qué generar números aleatorios en Rust?

Generar números aleatorios es una habilidad fundamental en la programación, ya sea para simular situaciones, realizar pruebas o aumentar la seguridad en las aplicaciones. En Rust, existen varias formas de generar números aleatorios de manera eficiente y segura, lo que lo convierte en un lenguaje ideal para esta tarea.

## Cómo hacerlo en Rust

Para generar números aleatorios en Rust, primero debemos importar la biblioteca "rand". Luego, podemos utilizar los métodos de esta biblioteca para generar diferentes tipos de números aleatorios, como enteros, decimales o booleanos.

```Rust
use rand::prelude::*; // Importar la biblioteca "rand"

// Generar un número entero aleatorio entre 1 y 10
let num: u32 = rand::thread_rng().gen_range(1, 11);
println!("Número aleatorio: {}", num);

// Generar un número decimal aleatorio entre 0 y 1
let num: f64 = rand::thread_rng().gen();
println!("Número aleatorio: {}", num);

// Generar un booleano aleatorio
let booleano: bool = rand::thread_rng().gen();
println!("Booleano aleatorio: {}", booleano);
```

## Profundizando en la generación de números aleatorios en Rust

En Rust, la generación de números aleatorios se basa en un generador llamado "Mersenne Twister". Este generador utiliza un estado interno para producir una secuencia de números aparentemente aleatorios, pero que siguen un patrón predecible. Además, Rust implementa medidas de seguridad para evitar que los números aleatorios sean adivinados por posibles atacantes.

Si deseamos generar números aleatorios con una distribución específica, podemos utilizar las funciones de la biblioteca "rand_distr". Por ejemplo, si deseamos generar un número aleatorio con distribución normal, podemos hacer lo siguiente:

```Rust
use rand::prelude::*;
use rand_distr::{Normal, Distribution}; // Importar biblioteca "rand_distr"

// Generar un número aleatorio con distribución normal
let distribucion = Normal::new(0.0, 1.0).unwrap();
let num: f64 = rand::thread_rng().sample(distribucion);
println!("Número aleatorio con distribución normal: {}", num);
```

## Ver también

- [Documentación de Rust sobre la generación de números aleatorios](https://doc.rust-lang.org/rand/)
- [Ejemplo de uso de diferentes funciones de la biblioteca "rand"](https://web.archive.org/web/20200710045717/https://rust-lang-nursery.github.io/rust-cookbook/science/mathematics/random.html)
- [Explicación detallada del generador Mersenne Twister](https://web.archive.org/web/20200710134450/https://doc.rust-lang.org/rand/rand/hc/struct.Hc128Core.html)