---
title:                "Rust: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué
Muchas veces en la programación necesitamos generar números aleatorios para diversas tareas, como por ejemplo para juegos, problemas de optimización o para pruebas. En este artículo vamos a explorar cómo podemos generar números aleatorios en Rust de una manera sencilla y eficiente.

## Cómo hacerlo
Para generar números aleatorios en Rust, vamos a utilizar el módulo `rand`, que nos proporciona herramientas para trabajar con números aleatorios. Primero, debemos agregar `rand` como dependencia en nuestro archivo `Cargo.toml`:

```
[rand]
version = "0.6.5"
```

Luego, en nuestro archivo `main.rs`, podemos importar el módulo `rand` de la siguiente manera:

```Rust
use rand::{thread_rng, Rng};
```

Ahora, para generar un número aleatorio, podemos utilizar la función `gen_range` de la siguiente manera:

```Rust
let numero = thread_rng().gen_range(1..11);
```

En este ejemplo, estamos generando un número aleatorio entre 1 y 10, ya que el límite superior no se incluye en el rango. Podemos usar esta misma función para generar números en otros rangos, como por ejemplo letras o cadenas de texto. También podemos generar números aleatorios con un tipo de dato específico, como `u32` o `f64`, especificándolo en la función.

Otra opción para generar números aleatorios es utilizar la función `gen()` del módulo `Rng`, que nos devuelve un número en el mismo tipo de dato que el valor proporcionado. Por ejemplo, si queremos generar un número aleatorio en el mismo rango que un `usize`, podemos hacerlo de esta manera:

```Rust
let numero = thread_rng().gen(1..=10);
```

## Profundizando
El módulo `rand` también nos ofrece otras opciones para generar números aleatorios, como por ejemplo la función `gen_bool` para generar valores booleanos aleatorios, o la función `shuffle` para mezclar un array de elementos de manera aleatoria.

También podemos utilizar la macro `random!` para generar un número aleatorio con un tipo de dato específico. Por ejemplo:

```Rust
let numero: f32 = random!(1..=10);
```

Otra opción interesante es utilizar la función `choose` para elegir un elemento aleatorio de un array o un `Vec`.

Y si queremos generar números aleatorios criptográficamente seguros, podemos utilizar el módulo `thread_rng` en lugar de `thread_rng`.

## Ver también
- [Documentación oficial de Rust sobre el módulo `rand`](https://doc.rust-lang.org/rand/)
- [Tutorial sobre números aleatorios en Rust](https://www.adamchalmers.com/tutorials/random-numbers-in-rust/)