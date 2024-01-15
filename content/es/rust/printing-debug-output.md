---
title:                "Imprimiendo salidas de depuración"
html_title:           "Rust: Imprimiendo salidas de depuración"
simple_title:         "Imprimiendo salidas de depuración"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Por qué imprimir salida de depuración?

A veces, cuando estás escribiendo código en Rust, puede ser difícil entender exactamente qué está sucediendo detrás de escena. Imprimir salida de depuración es una forma útil de ver los valores de las variables en diferentes puntos de tu código y entender cómo se están manipulando.

## Cómo hacerlo

La forma más sencilla de imprimir salida de depuración en Rust es utilizar el macro `dbg!()`. Aquí tienes un ejemplo:

```Rust
let x = 5;
let y = 10;

dbg!(x);
dbg!(y);
```

Este código imprimirá la siguiente salida:

```
[src/main.rs:3] x = 5
[src/main.rs:4] y = 10
```

Como puedes ver, la salida contiene la ubicación en el código donde se imprime la variable y el valor actual de la misma. Esto puede ser muy útil para entender el flujo del programa y encontrar posibles errores.

## Profundizando

Además del macro `dbg!()`, también puedes utilizar el método `println!()` para imprimir salida de depuración. La diferencia con `dbg!()` es que con `println!()` necesitas especificar el formato de la variable que quieres imprimir.

```Rust
let x = 5;
let y = 10;

println!("La variable x es igual a {}", x);
println!("La variable y es igual a {}", y);
```

La salida será la misma que con `dbg!()`:

```
La variable x es igual a 5
La variable y es igual a 10
```

Además, puedes utilizar el atributo `#[derive(Debug)]` en tus estructuras y enumeraciones para añadir automáticamente la posibilidad de imprimir su contenido con `println!()` o `dbg!()`.

## Ver también

- [Documentación de Rust sobre imprimir salida de depuración](https://doc.rust-lang.org/std/macro.dbg.html)
- [Tutorial sobre cómo imprimir salida de depuración en Rust](https://www.ameyalokare.com/rust/2017/10/09/rust-debug-macros-adventures.html)
- [Pautas para depurar con impresiones en Rust](https://www.suspectsemantics.com/blog/2016/02/13/better-rust-debugging.html)