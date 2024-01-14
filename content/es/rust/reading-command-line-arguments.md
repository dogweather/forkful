---
title:                "Rust: Leyendo argumentos de línea de comando"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

Los argumentos de línea de comandos son una forma útil de interactuar con un programa de Rust. Ya sea para proporcionar opciones o datos adicionales, entender cómo leer y utilizar estos argumentos puede mejorar la funcionalidad y la eficiencia de tu código.

## Cómo hacerlo

Para leer los argumentos de línea de comandos en Rust, puedes utilizar la función `args()` del módulo `std::env`. Esta función devuelve un iterador que puedes recorrer para obtener los argumentos proporcionados al programa.

Por ejemplo, si deseas imprimir en pantalla todos los argumentos que se pasaron al programa, puedes hacerlo de la siguiente manera:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    for argumento in args {
        println!("{}", argumento);
    }
}
```

Supongamos que el programa se llama `mi-programa` y se ejecuta de la siguiente manera:

```bash
$ mi-programa hola mundo
```

El resultado impreso en pantalla será:

```
mi-programa
hola
mundo
```

## Profundizando

Además de leer los argumentos proporcionados, también puedes realizar otras operaciones con ellos. Por ejemplo, puedes acceder a un argumento específico utilizando su índice en el vector de argumentos, como se muestra a continuación:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let primer_argumento = &args[1];
    println!("El primer argumento es: {}", primer_argumento);
}
```

De esta forma, si ejecutas el programa `mi-programa` de la siguiente manera:

```bash
$ mi-programa hola mundo
```

La salida será:

```
El primer argumento es: hola
```

## Ver también

- Documentación oficial de `std::env`: https://doc.rust-lang.org/std/env/
- Tutorial de lectura de argumentos en Rust: https://www.rust-lang.org/learn/get-started#command-line-arguments
- Ejemplos de programas que utilizan argumentos de línea de comandos: https://github.com/rust-lang/rust-by-example#command-line