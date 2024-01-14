---
title:    "Rust: Leyendo argumentos de línea de comandos"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué
A la hora de escribir un programa, muchas veces se desea recibir información del usuario directamente desde la línea de comandos. En este post, aprenderemos cómo leer argumentos de línea de comandos en Rust para poder realizar acciones personalizadas en nuestros programas.

## Cómo Hacerlo
La lectura de argumentos de línea de comandos en Rust es muy sencilla gracias a la librería estándar `std::env`. Para utilizarla, simplemente debemos importarla al principio de nuestro programa:

```Rust
use std::env;
```

Luego, podemos utilizar la función `args()` para obtener una lista de los argumentos pasados por línea de comandos:

```Rust
let args: Vec<String> = env::args().collect();
```

Ahora, si ejecutamos nuestro programa desde la línea de comandos con algunos argumentos, por ejemplo:

```bash
./mi_programa arg1 arg2 arg3
```

La variable `args` tendrá una lista con los mismos argumentos en el mismo orden. Podemos acceder a ellos utilizando su índice, por ejemplo:

```Rust
let primer_arg = &args[1];
let tercer_arg = &args[3];
```

También podemos utilizar la función `len()` para obtener la cantidad de argumentos recibidos, y el bucle `for` para recorrerlos todos:

```Rust
for argumento in args.iter() {
    println!("{}", argumento);
}
```

De esta forma, podemos utilizar los argumentos de línea de comandos para modificar el comportamiento de nuestro programa.

## Inmersión Profunda
Además de los argumentos que el usuario pasa a través de la línea de comandos, también es posible obtener información del entorno en el que se está ejecutando el programa. Esto se puede hacer utilizando la función `var()` de la librería `std::env`, que nos permite obtener el valor de una variable de entorno específica.

Por ejemplo, si queremos obtener el nombre de usuario del usuario que está ejecutando nuestro programa, podemos utilizar la variable de entorno `USER`:

```Rust
let usuario = env::var("USER").unwrap();
println!("El usuario actual es {}", usuario);
```

## Ver También
- [Documentación de `std::env` - Rust](https://doc.rust-lang.org/std/env/index.html)
- [Tutorial: Argumentos de línea de comandos en Rust](https://www.rust-lang.org/learn/cli-arguments)