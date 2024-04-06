---
date: 2024-01-20 17:56:44.858783-07:00
description: "C\xF3mo: Ejecuci\xF3n en terminal y salida esperada."
lastmod: '2024-04-05T21:54:00.209680-06:00'
model: gpt-4-1106-preview
summary: "Ejecuci\xF3n en terminal y salida esperada."
title: "Lectura de argumentos de l\xEDnea de comandos"
weight: 23
---

## Cómo:
```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    // Asumiendo que el primer argumento es el nombre del programa
    if args.len() > 1 {
        println!("Argumentos recibidos:");
        for arg in args.iter().skip(1) {
            println!("{}", arg);
        }
    } else {
        println!("No se han proporcionado argumentos.");
    }
}
```

Ejecución en terminal y salida esperada:
```
$ my_app argumento1 argumento2
Argumentos recibidos:
argumento1
argumento2
```

## Análisis Profundo:
Históricamente, leer argumentos de la línea de comandos es una práctica fundamental en la mayoría de lenguajes de programación que interactúan con shells de Unix y Windows. Rust proporciona la librería estándar `std::env`, y con ella, las funciones necesarias. Alternativas incluyen crates como `clap` y `structopt` que permiten una gestión de argumentos más avanzada y con mejor ergonomía.

Hablando de implementación, `env::args()` retorna un iterador que produce cada argumento como `String`. Es importante notar que el primer elemento siempre es el path hacia el binario que se está ejecutando. Por eso, usamos `.skip(1)` para empezar a procesar argumentos desde el usuario. En aplicaciones más complejas, es común desglosar y tratar distintos tipos de argumentos con diferentes comportamientos y validaciones.

## Ver También:
- Documentación oficial de `std::env`: https://doc.rust-lang.org/std/env/
- Crates para manejar argumentos de línea de comandos:
  - Clap: https://crates.io/crates/clap
  - StructOpt: https://crates.io/crates/structopt
- Una guía para el manejo de argumentos de línea de comando en Rust: https://rust-lang-nursery.github.io/cli-wg/tutorial/arguments.html
