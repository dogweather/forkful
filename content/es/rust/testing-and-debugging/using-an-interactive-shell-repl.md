---
date: 2024-01-26 04:17:40.429083-07:00
description: "C\xF3mo hacerlo: Por ahora, Rust no tiene un REPL oficial incluido.\
  \ Puedes usar herramientas de terceros como `evcxr_repl`. Inst\xE1lalo con Cargo."
lastmod: '2024-03-13T22:44:58.848200-06:00'
model: gpt-4-0125-preview
summary: Por ahora, Rust no tiene un REPL oficial incluido.
title: Usando una shell interactiva (REPL)
weight: 34
---

## Cómo hacerlo:
Por ahora, Rust no tiene un REPL oficial incluido. Puedes usar herramientas de terceros como `evcxr_repl`. Instálalo con Cargo:

```sh
cargo install evcxr_repl
```

Luego, ejecuta el REPL:

```sh
evcxr
```

Dentro, prueba algo de código Rust:

```rust
let x = 5;
let y = 3;
println!("{} + {} = {}", x, y, x + y);
```

La salida debería ser:

```
5 + 3 = 8
```

## Análisis Profundo
El ethos de Rust se centra en la seguridad y el rendimiento, que generalmente se asocian con lenguajes compilados previamente, y menos con lenguajes interpretados, amigables con REPL. Históricamente, lenguajes como Python o Ruby priorizaron tener un REPL para obtener retroalimentación inmediata, pero no fueron diseñados con tareas a nivel de sistema en mente.

A pesar de la ausencia de un REPL oficial en Rust, han surgido un par de alternativas como `evcxr_repl`. Estos proyectos no solo están adaptando Rust a un REPL; están tejiendo inteligentemente el ciclo de compilar y ejecutar del lenguaje en una sesión interactiva. El REPL compila el código detrás de escena y ejecuta el binario, capturando la salida. De esta manera, preserva los beneficios de rendimiento de Rust mientras aún ofrece esa experiencia interactiva.

Hay una discusión en curso en la comunidad de Rust sobre el soporte oficial de REPL, y con cada iteración del lenguaje, vemos más sofisticación en las herramientas que eventualmente podrían conducir a una solución nativa.

## Ver También
Para más información y otras herramientas:
- Repositorio GitHub de Evcxr REPL: [https://github.com/google/evcxr](https://github.com/google/evcxr)
- Rust Playground, una forma en línea de probar código Rust: [https://play.rust-lang.org/](https://play.rust-lang.org/)
- Discusión en el lenguaje Rust sobre la característica REPL: [https://internals.rust-lang.org/](https://internals.rust-lang.org/)
