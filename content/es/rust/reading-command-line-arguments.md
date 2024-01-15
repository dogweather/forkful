---
title:                "Leyendo argumentos de línea de comando"
html_title:           "Rust: Leyendo argumentos de línea de comando"
simple_title:         "Leyendo argumentos de línea de comando"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

Si estás aprendiendo Rust o ya eres un programador experimentado, saber cómo trabajar con argumentos de línea de comandos es una habilidad importante. Te permitirá construir aplicaciones más versátiles y flexibles, además de mejorar tu conocimiento sobre Rust como lenguaje de programación.

## Cómo hacerlo

Para leer argumentos de línea de comandos en Rust, necesitas importar el módulo "std::env" y llamar a la función "args()". Luego, puedes utilizar el método "collect()" para obtener un vector que contiene cada argumento ingresado desde la línea de comandos. A continuación, puedes iterar sobre el vector y usar los argumentos según sea necesario.

```
Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    for arg in args.iter() {
        println!("{}", arg);
    }
}
```

Si ejecutamos este código con el siguiente comando en nuestra terminal:

```
cargo run hola mundo
```

Obtendremos la siguiente salida:

```
hola
mundo
```

## Profundizando

Además de leer argumentos de línea de comandos simples, también puedes manejar argumentos con opciones y valores. Por ejemplo, puedes utilizar el paquete "clap" para crear una interfaz de línea de comandos más robusta.

```
Rust
use clap::{Arg, App};

fn main() {
   let matches = App::new("mi_app")
       .arg(Arg::with_name("opcion")
           .short("o")
           .long("opcion")
           .value_name("OPCION")
           .help("La opción que deseas usar")
           .takes_value(true))
       .get_matches();

   if let Some(opcion) = matches.value_of("opcion") {
       println!("Usaste la opción {}", opcion);
   }
}
```

Con este código, podemos ejecutar nuestro programa con la siguiente línea de comandos: `cargo run --opcion hola`

Y obtendremos la siguiente salida:

```
Usaste la opción hola
```

## Ver también

- [Documentación oficial sobre argumentos de línea de comandos en Rust](https://doc.rust-lang.org/std/env/fn.args.html)
- [Tutorial sobre el uso de argumentos de línea de comandos en Rust](https://www.tutorialspoint.com/rust/rust_command_line_arguments.htm)
- [Paquete "clap" en crates.io para manejar opciones de línea de comandos en Rust](https://crates.io/crates/clap)

¡Ahora que sabes cómo trabajar con argumentos de línea de comandos en Rust, ve y crea aplicaciones increíbles!