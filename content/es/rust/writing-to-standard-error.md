---
title:                "Rust: Escribiendo en el error estándar"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

Escribir a la salida de error estándar (standard error) es una práctica común en la programación en Rust. Aunque puede parecer una tarea simple, entender por qué es importante y cómo hacerlo correctamente es fundamental para escribir un código limpio y eficiente.

## Cómo hacerlo

Para escribir a la salida de error estándar en Rust, podemos utilizar la macro `eprintln!` seguida del mensaje que queremos imprimir. Por ejemplo:

```Rust
eprintln!("Este es un mensaje de error");
```

Esta macro imprimirá el mensaje en la pantalla, con un formato similar al siguiente:

```sh
error: Este es un mensaje de error
```

También podemos utilizar la macro `write!` para escribir a la salida de error en lugar de la salida estándar. Esta macro se utiliza de la misma forma que `eprintln!` pero debe ser seguida de un `stream` que represente la salida de error estándar. Por ejemplo:

```Rust
use std::io::Write;

fn main() {
    let mut stderr = std::io::stderr();
    write!(&mut stderr, "Este es un mensaje de error").unwrap();
}
```

## Profundizando

Cuando escribimos a la salida de error en Rust, estamos enviando mensajes a un `stream` diferente que el de la salida estándar. Esto significa que los mensajes de error se pueden mostrar en un orden diferente que el de la salida estándar. Además, también podemos utilizar la función `panic!` para imprimir mensajes en la salida de error antes de terminar la ejecución del programa.

Otra ventaja de escribir a la salida de error es que podemos capturar estos mensajes y utilizarlos para realizar acciones específicas en nuestro código. Por ejemplo, si queremos imprimir un mensaje de error personalizado y luego terminar la ejecución del programa, podemos utilizar la macro `panic!` junto con la función `unwrap()` o `expect()`. Esto nos permite controlar cómo manejamos los errores en nuestro código y proporcionar una experiencia más amigable para el usuario.

## Ver también

- Documentación de Rust sobre las macros `eprintln!` y `write!`: https://doc.rust-lang.org/std/macro.eprintln.html y https://doc.rust-lang.org/std/macro.write.html
- Ejemplos de uso de la macro `panic!`: https://doc.rust-lang.org/std/macro.panic.html
- Ejemplo de uso de la función `unwrap()`: https://doc.rust-lang.org/std/result/enum.Result.html#method.unwrap
- Ejemplo de uso de la función `expect()`: https://doc.rust-lang.org/std/result/enum.Result.html#method.expect