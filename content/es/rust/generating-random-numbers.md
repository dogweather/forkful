---
title:                "Generando números aleatorios"
date:                  2024-01-20T17:49:51.144792-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generando números aleatorios"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Generar números aleatorios es como sacar un número de la galera, pero en tu código. Los programamos para todo: desde decidir quién va primero en un juego hasta simular cómo se comportan los mercados financieros. Es esencial para darle sabor al azar en nuestros proyectos.

## Cómo Hacerlo:
Para generar números aleatorios en Rust, necesitas la caja (o crate, en inglés) `rand`. Asegurate de incluirla en tu `Cargo.toml`.

```rust
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    let numero_aleatorio: i32 = rng.gen_range(1..101);
    println!("Aquí tienes un número aleatorio entre 1 y 100: {}", numero_aleatorio);
}
```

Ejecuta tu código y obtendrás algo parecido a esto:

```
Aquí tienes un número aleatorio entre 1 y 100: 47
```

Recuerda, cada vez que lo corres, el número será diferente.

## Inmersión Profunda:
La generación de números aleatorios no es algo nuevo; ha estado en la programación desde sus comienzos. En Rust, la caja `rand` es la herramienta de facto para conseguir aleatoriedad. Internamente, elige distintos métodos para generar esa sensación de azar, dependiendo si buscas rapidez, criptografía o reproducción exacta.

Hay alternativas como `fastrand` si quieres algo más minimalista o `rand_xorshift` si te interesan algoritmos específicos de generación de números pseudorandom.

Pero, ¿por qué no usar simplemente la función `rand()` de C? En Rust, la seguridad y la expresividad son claves. La caja `rand` fue diseñada para ser segura al uso y flexible para que el manejo de la aleatoriedad sea robusto y confiable.

## Ver También:
- Una discusión sobre diferentes generadores de números aleatorios en Rust [The Rust Rand Book](https://rust-random.github.io/book/)
- Un post sobre generación de aleatoriedad y sus principios en [The Rust Programming Language Forum](https://users.rust-lang.org/)

Cada recurso te dará más contexto y ejemplos para que generes números aleatorios como todo un profesional. ¡Buena suerte experimentando con la aleatoriedad en tus proyectos!
