---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
Filtra argumentos de la línea de comandos es leer inputs directamente ingresados al ejecutar tu programa. Le da a los programadores una forma eficiente de personalizar la ejecución del código sin cambiarlo.

## ¿Cómo hacerlo?
Rust hace que leer argumentos de la línea de comandos sea un proceso sencillo. Mira el siguiente código en el que iteramos sobre la colección `args` devuelta por `std::env::args`.

```Rust
fn main() {
    for argumento in std::env::args() {
        println!("{}", argumento);
    }
}
```
Si ejecutas este código con `cargo run uno dos tres`, verás esta salida:

```Rust
ruta/a/programa
uno
dos
tres
```

Aquí, `ruta/a/programa` es el argumento 0, que siempre es el camino al ejecutable. Los sucesivos argumentos son el resto de inputs.

## Profundización
Desde una perspectiva histórica, leer argumentos desde la línea de comandos se realiza desde los primeros días de la programación, es una técnica útil que sobrevive hoy en día.

En lo que respecta a alternativas, Rust ofrece la biblioteca `getopts`. Esta proporciona funciones para analizar argumentos de línea de comando de una manera más detallada. Pero para la mayoría de los casos de uso, `std::env::args` funciona perfectamente.

El funcionamiento interno de `std::env::args` es bastante sencillo. Al iniciar el programa, el sistema operativo pasa todos los argumentos al programa como una lista. Luego, Rust simplemente accede a esta lista.

## Ver además
Para más información, consulta la documentación oficial de Rust sobre [std::env](https://doc.rust-lang.org/std/env/index.html) y la biblioteca [`getopts`](https://docs.rs/getopts/0.2.21/getopts/). Encontrarás información más detallada y actualizada allí. También puedes consultar el [book de Rust](https://doc.rust-lang.org/book/) para entender mejor cómo Rust maneja los argumentos de la línea de comando.