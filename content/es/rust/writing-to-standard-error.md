---
title:    "Rust: Escribiendo en el error estándar"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Por qué

Escribir a la salida de error estándar es una práctica común en la programación, especialmente en lenguajes como Rust. Esto se debe a que al escribir en esta salida, se pueden mostrar mensajes de error o información de depuración durante la ejecución del programa. Además, también permite al usuario redirigir los mensajes de error a diferentes archivos o sistemas de registro, lo que facilita la identificación y solución de problemas en la aplicación.

## Cómo hacerlo

Para escribir a la salida de error estándar en Rust, se utiliza la macro `eprintln!()`. Esta macro acepta un argumento de cadena y utiliza el formato de impresión para mostrar mensajes específicos. Por ejemplo:

```Rust
eprintln!("Error al conectarse al servidor. Código de error: {}", code);
```

Los mensajes escritos con `eprintln!()` aparecerán en la consola durante la ejecución del programa.

## Profundizando

Además de la macro `eprintln!()`, también es importante conocer la diferencia entre estándar error y estándar output. Mientras que la salida de error se usa para mostrar mensajes de error y depuración, la salida estándar se utiliza para información general y mensajes de éxito. En Rust, la salida estándar se maneja con la macro `println!()`.

Otra función útil para escribir en la salida de error es `std::io::stderr()`. Esta función permite crear un objeto `Stderr` que se puede utilizar para escribir directamente a la salida de error. Por ejemplo:

```Rust
use std::io::Write;

let mut stderr = std::io::stderr();
writeln!(stderr, "Ocurrió un error en la línea {} del código", line);
```

Este método es particularmente útil para imprimir mensajes de error en un formato estructurado.

# Ver también

- [Documentación oficial de Rust sobre escritura de mensajes de error](https://doc.rust-lang.org/std/io/struct.Stdin.html#method.stderr)
- [Tutorial de Rust sobre manejo de errores](https://www.rust-lang.org/learn/get-started)
- [Artículo sobre el uso de salidas de error estándar en Rust](https://medium.com/@fredrikanderzon/rust-standard-output-and-standard-error-e1a7cf3086b9)