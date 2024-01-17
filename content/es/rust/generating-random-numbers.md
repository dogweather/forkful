---
title:                "Generando números aleatorios"
html_title:           "Rust: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué se hace?

Generar números aleatorios es una técnica utilizada por los programadores para producir valores numéricos aleatorios con propósitos diversos, como juegos, pruebas y criptografía. Al generar números aleatorios, se puede simular diferentes escenarios y crear resultados impredecibles, lo que puede ser útil en muchas aplicaciones.

## Cómo hacerlo:

La biblioteca estándar de Rust proporciona la macro ```rand::random```, que devuelve un valor de tipo ```u64``` aleatorio generado por el generador de números aleatorios global (PRNG). Se pueden especificar tipos específicos agregando una anotación de tipo antes de la llamada a la macro. Por ejemplo, ```let random_number: i32 = rand::random();``` generará un valor entero aleatorio de 32 bits.

## Exploración profunda

La generación de números aleatorios ha sido un área de investigación en la informática desde sus inicios. Algunos de los primeros algoritmos incluyen el método del cuadrado medio, el método del producto medio y el método de congruencia lineal. Hay muchas alternativas al PRNG global implementado en Rust, como el Mersenne Twister, que es conocido por producir secuencias de números aleatorios de alta calidad.

## Ver también:

- Documentación oficial de Rust sobre generación de números aleatorios: https://doc.rust-lang.org/book/ch07-06-managing-growing-projects-with-packages-crates-and-modules.html#generating-random-numbers 
- El libro "Introduction to Randomness and Random Numbers" de Miguel Sanchez: http://random.mat.sbg.ac.at/people/sk/2011/sk-isc-2011.pdf
- Pseudo-random number generator en Wikipedia: https://en.wikipedia.org/wiki/Pseudorandom_number_generator