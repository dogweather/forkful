---
title:                "Rust: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

Comprobar si un directorio existe es una tarea común en la programación. Con Rust, puedes hacerlo de forma eficiente y segura gracias a su gestión de memoria sin errores y su sistema de tipos fuertes.

## Cómo hacerlo

Para comprobar si un directorio existe en Rust, utilizaremos la librería `std::fs` y su función `metadata`. Esta función nos devuelve información sobre el archivo o directorio en cuestión.

Primero, importamos la librería al inicio de nuestro código:
 
```Rust
use std::fs; 
```

Luego, creamos una función que tome como parámetro la ruta del directorio que queremos comprobar:

```Rust
fn check_directory(directory: &str) { 
    let metadata = fs::metadata(directory); 
    match metadata { 
        Ok(metadata) => { 
            if metadata.is_dir() { 
                println!("El directorio {} existe.", directory); 
            } else { 
                println!("El directorio {} no existe.", directory); 
            } 
        }, 
        Err(e) => println!("Error: {}", e), 
    } 
}
```

Aquí utilizamos un `match` para manejar tanto el caso en el que el directorio exista como el caso en el que no exista. También podemos utilizar `if let` en lugar de `match` para escribir un código más corto, pero menos explícito.

Finalmente, llamamos a nuestra función y le pasamos la ruta del directorio que queremos comprobar:

```Rust
fn main() { 
    check_directory("/home/usuario/Documentos"); 
}
```

Si el directorio existe, obtendremos la siguiente salida:

```
El directorio /home/usuario/Documentos existe.
```

Si el directorio no existe, la salida será la siguiente:

```
El directorio /home/usuario/Documentos no existe.
```

## Profundizando

La función `metadata` utiliza una llamada al sistema para obtener información sobre el archivo o directorio. Si el directorio no existe, la función devolverá un error `NotFound`. Podemos utilizar el tipo de dato `std::fs::Metadata` para acceder a más información sobre el directorio, como su tamaño y fecha de creación.

También podemos utilizar la función `fs::read_dir` para obtener una lista de los archivos y directorios contenidos en un directorio específico.

## Ver también

- [Documentación oficial de std::fs](https://doc.rust-lang.org/std/fs/index.html)
- [Tutorial de Rust para principiantes](https://www.rust-lang.org/learn/get-started)
- [Ejemplos de código de Rust](https://github.com/rust-lang/rust-by-example)