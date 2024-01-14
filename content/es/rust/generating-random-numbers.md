---
title:    "Rust: Generando números aleatorios"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## ¿Por qué usar Rust para generar números aleatorios?

La generación de números aleatorios es una tarea común en la programación, ya sea para juegos, simulaciones o criptografía. En este artículo, exploraremos cómo generar números aleatorios de manera eficiente y confiable en Rust, un lenguaje de programación moderno y potente.

## Cómo hacerlo

La generación de números aleatorios en Rust es muy sencilla gracias al módulo `rand` incluido en la biblioteca estándar. Para comenzar, importamos el módulo con la siguiente línea de código:

```Rust
use rand::Rng;
```

Luego, podemos generar un número aleatorio utilizando el generador `thread_rng()` y especificando un rango como parámetro. Por ejemplo, si queremos generar un número entre 1 y 10, podemos hacerlo de la siguiente manera:

```Rust
let num = thread_rng().gen_range(1..=10); //genera un número aleatorio entre 1 y 10
```

También podemos generar un vector de números aleatorios utilizando la función `gen()` del generador. Por ejemplo, si queremos 5 números aleatorios entre 1 y 100, podemos hacerlo de esta forma:

```Rust
let nums = thread_rng().gen::<i32>(); //genera un vector de 5 números aleatorios
```

## Profundizando

Para entender mejor cómo funciona la generación de números aleatorios en Rust, es útil conocer los algoritmos utilizados detrás de escena. En Rust, se utilizan dos algoritmos principales: `Xorshift` y `Hc128`.

El algoritmo `Xorshift` es muy rápido y se utiliza para generaciones de números aleatorios simples. Funciona a través de una operación XOR entre un valor inicial y un número constante, y luego se utiliza ese resultado como el valor inicial para la próxima generación. Sin embargo, este algoritmo no garantiza valores completamente aleatorios, por lo que no es adecuado para usos criptográficos.

Por otro lado, el algoritmo `Hc128` es más lento pero ofrece una mejor aleatoriedad. Este algoritmo utiliza un generador más complejo basado en operaciones de desplazamiento y rotación en un espacio de 128 bits. Aunque es más adecuado para usos criptográficos, puede ser un poco excesivo para tareas simples de generación de números aleatorios.

## Ver también

- Documentación oficial del módulo `rand` de Rust: https://doc.rust-lang.org/rand/rand/index.html
- Ejemplos de generación de números aleatorios en Rust: https://github.com/rust-lang-nursery/rand/tree/master/rand/examples
- Artículo sobre el uso de la generación de números aleatorios en criptografía: https://medium.com/@joshuaduffy/how-cryptographically-secure-are-rand-and-os-rand-in-rust-939d5458f0