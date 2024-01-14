---
title:    "Rust: Leyendo argumentos de línea de comandos"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por qué

El manejo de argumentos de línea de comando es una habilidad esencial para cualquier programador de Rust. Esta técnica te permitirá crear aplicaciones más versátiles y personalizables, y también te ayudará a mejorar tu comprensión de Rust.

## Cómo hacerlo

Para leer los argumentos de línea de comando en Rust, se utiliza el módulo "std::env". Primero, se importa el módulo con la siguiente línea de código: 

```Rust 
use std::env; 
```

Luego, se utiliza la función "args" del módulo para obtener un iterator que contiene los argumentos ingresados en la línea de comando. Por ejemplo, si el usuario ingresa "cargo run [argumento1] [argumento2]", se obtendrá un iterator con los argumentos [argumento1] y [argumento2]. 

Para acceder a estos argumentos, se utiliza el método "next" en el iterator. En cada llamada, este método devuelve un "Option" que puede ser "Some" si hay un argumento presente, o "None" si ya se han leído todos los argumentos. Por lo tanto, se utiliza un "match" para procesar los argumentos de forma adecuada. 

A continuación, se muestra un ejemplo de código que lee dos argumentos de línea de comando e imprime su valor en la consola:

```Rust 
use std::env;

fn main() {
    let mut args = env::args();
    args.next(); // Se descarta el primer argumento que es el nombre del programa
    // Se leen los dos argumentos ingresados en la línea de comando
    let arg1 = args.next().unwrap();
    let arg2 = args.next().unwrap();
    
    println!("El primer argumento es: {}", arg1);
    println!("El segundo argumento es: {}", arg2);
}
```

El resultado de ejecutar este programa con los argumentos "Hola" y "Mundo" sería:

```
El primer argumento es: Hola
El segundo argumento es: Mundo
```
## Profundizando más

Además de leer los argumentos de línea de comando básicos, Rust ofrece otras funcionalidades que pueden ser de utilidad. Una de ellas es la posibilidad de especificar opciones y parámetros en los argumentos. Esto se logra utilizando el módulo "clap", que ofrece una sintaxis más amigable y un manejo más avanzado de los argumentos. 

También es posible manejar errores al leer los argumentos utilizando el módulo "std::env::ArgsOs", que devuelve un iterator que contiene objetos tipo "OsString" en lugar de "String". Esto permite manejar de forma segura los argumentos que contienen caracteres unicode o con espacios en blanco.

## Ver también

- Documentación oficial de Rust sobre el módulo "std::env": https://doc.rust-lang.org/std/env/index.html
- Ejemplos de uso del módulo "std::env": https://github.com/rust-lang/rust-by-example/blob/master/flow_control/command_line_arguments.md
- Documentación del módulo "clap": https://docs.rs/clap/2.33.0/clap/