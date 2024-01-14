---
title:    "Rust: Generación de números aleatorios"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué
Generar números aleatorios es una tarea común en la programación y puede ser útil en una variedad de aplicaciones, desde juegos hasta pruebas de rendimiento. Aprender a generar números aleatorios en Rust puede ser beneficioso para ampliar tus habilidades de programación y explorar el lenguaje en profundidad.

## Cómo
Para generar números aleatorios en Rust, puedes utilizar la función `rand::random()` de la biblioteca estándar. Esta función devuelve un número aleatorio de cualquier tipo que implemente el trait `Rand`, como `u32` o `f64`. A continuación se muestra un ejemplo de cómo generar un número aleatorio entre 1 y 10 y mostrarlo en pantalla:

```Rust
use rand::Rng; // Importar la trait Rng

fn main() {
    let num = rand::random::<u32>() % 10 + 1; // Generar número aleatorio entre 1 y 10
    println!("{}", num); // Imprimir número en pantalla
}
```

La función `Rng::gen_range()` también puede ser utilizada para generar números aleatorios en un rango específico. Por ejemplo, para generar un número aleatorio entre 50 y 100:

```Rust
use rand::Rng; // Importar la trait Rng

fn main() {
    let mut rng = rand::thread_rng(); // Crear un nuevo generador de números aleatorios
    let num = rng.gen_range(50, 101); // Generar número aleatorio entre 50 y 100
    println!("{}", num); // Imprimir número en pantalla
}
```

## Deep Dive
La función `rand::random()` utiliza el generador de números aleatorios `ThreadRng` que se inicializa utilizando una semilla única basada en el tiempo actual. Esto significa que si la función se llama varias veces en un corto período de tiempo, es probable que los números generados sean muy similares. Una solución para este problema es crear un generador de números aleatorios personalizado y utilizarlo en lugar de `ThreadRng`.

Además, la biblioteca `rand` también proporciona varios algoritmos de generación de números aleatorios más complejos, como el generador `Isaac64`, que utiliza el algoritmo ISAAC diseñado para ser utilizado en aplicaciones criptográficas. Estos algoritmos pueden ser útiles en casos donde se requieren mayores niveles de aleatoriedad y seguridad.

## Ver también
- Documentación oficial de Rust sobre la biblioteca `rand`: https://doc.rust-lang.org/rand/
- Tutorial sobre cómo generar números aleatorios en Rust: https://dev.to/itnext/generating-random-numbers-in-rust-12ae
- Ejemplos de uso de la biblioteca `rand` en proyectos reales: https://github.com/rust-random/rand/tree/master/examples