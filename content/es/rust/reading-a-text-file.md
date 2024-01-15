---
title:                "Leyendo un archivo de texto."
html_title:           "Rust: Leyendo un archivo de texto."
simple_title:         "Leyendo un archivo de texto."
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has tenido que lidiar con un montón de datos almacenados en un archivo de texto? ¿Te has preguntado cómo podrías analizar esos datos de manera eficiente y sin errores humanos? Bueno, ¡has venido al lugar correcto! En este artículo te mostraré cómo puedes leer un archivo de texto utilizando el lenguaje de programación Rust.

## Cómo hacerlo

Para leer un archivo de texto en Rust, necesitarás importar el módulo `std::fs` y usar la función `read_to_string`. Por ejemplo, si queremos leer un archivo llamado "datos.txt" que se encuentra en el mismo directorio que nuestro programa, el código se vería así:

```Rust
use std::fs;
let data = fs::read_to_string("datos.txt").expect("No se pudo leer el archivo");
println!("{}", data);
```

Este código primero importa el módulo `std::fs` y luego usa la función `read_to_string` para leer el contenido del archivo "datos.txt" y almacenarlo en la variable `data`. Luego, utilizamos la función `println` para imprimir el contenido del archivo en la consola. Pero, ¿qué pasa si queremos hacer algo más con los datos del archivo? ¡Sigamos leyendo!

## Profundizando

La función `read_to_string` devuelve una cadena (`String`) que contiene todo el contenido del archivo de texto. Esto significa que podemos aplicar métodos de cadena (como `split` o `trim`) para manipular los datos como queramos. Por ejemplo, si nuestro archivo de texto contiene una lista de nombres separados por comas, podemos usar la función `split` para crear un vector que contenga cada nombre por separado:

```Rust
use std::fs;
let data = fs::read_to_string("datos.txt").expect("No se pudo leer el archivo");
let nombres: Vec<&str> = data.split(",").collect();
println!("{:?}", nombres);
```

En este código, utilizamos el método `split` para dividir la cadena en diferentes elementos cada vez que encuentre una coma. Luego, utilizamos el método `collect` para almacenar estos elementos en un vector de cadenas (`Vec<&str>`). Finalmente, imprimimos el vector en la consola. Los métodos de cadena en Rust son muy poderosos y te permiten manipular los datos de manera eficiente y segura.

## Ver también

Si quieres seguir aprendiendo más sobre cómo trabajar con archivos en Rust, te recomiendo que eches un vistazo a estos recursos:

- La documentación oficial de Rust sobre el módulo `std::fs` para obtener más información sobre cómo trabajar con archivos: https://doc.rust-lang.org/std/fs/
- Este tutorial de Rust sobre cómo leer y escribir archivos: https://doc.rust-lang.org/book/ch12-00-an-io-project.html#reading-a-file
- El libro "Programming Rust" de Jim Blandy y Jason Orendorff, que contiene una sección dedicada al manejo de archivos en Rust: https://www.oreilly.com/library/view/programming-rust/9781491927274/

¡Espero que este artículo te haya sido útil y que te haya animado a seguir explorando todo lo que Rust tiene para ofrecer! ¡Hasta la próxima!