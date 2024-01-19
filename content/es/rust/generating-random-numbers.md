---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

# ¡Aventurémonos en Rust: Generación de Números Aleatorios!
## ¿Qué & Por qué?
La generación de números aleatorios implica producir números que no siguen ningún patrón predecible. Los programadores utilizan números aleatorios para tareas como simulaciones, cifrado o simplemente juegos.

## Cómo hacer:
Esto es cómo generar un número aleatorio en Rust.

```Rust
use rand::Rng;
fn main() {
    let mut rng = rand::thread_rng();
    let n: u32 = rng.gen_range(0, 10);
    println!("Número aleatorio: {}", n);
}
```

Cuando lo ejecutas, puedes conseguir algo así:

```Rust
"Número aleatorio: 4"
```

## Buceo Profundo:
La generación de números aleatorios tiene un largo historial en la informática. Rust utiliza el paquete `rand` que proporciona una implementación eficaz y fácil de usar.

Alternativamente, puedes usar el paquete `rand::distributions` que ofrece diferentes formas de generación de números aleatorios, como distribución uniforme, normal, etc.

La clave de la generación de números aleatorios en Rust es la interfaz del generador de números aleatorios, `Rng`. `Rng` define métodos que generan números aleatorios de diferentes formas.

## Ver También:
1. Rust `rand` Crate Documentation: [https://docs.rs/rand](https://docs.rs/rand)
2. Generación de Números Aleatorios en Rust: [https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html](https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html)
3. Métodos para la Generación de Números Aleatorios: [https://en.wikipedia.org/wiki/Random_number_generation](https://en.wikipedia.org/wiki/Random_number_generation)