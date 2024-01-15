---
title:                "Escribiendo a error estándar"
html_title:           "Rust: Escribiendo a error estándar"
simple_title:         "Escribiendo a error estándar"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

Escribir a la salida de error estándar (standard error) puede ser útil en situaciones en las que se necesita enviar un mensaje de error al usuario o registrar información detallada durante la ejecución de un programa.

## Cómo hacerlo

Para escribir a la salida de error estándar en Rust, se utiliza la macro `eprintln!` seguida del mensaje que se desea imprimir. Por ejemplo:

```rust
eprintln!("¡Error! Algo salió mal.");
```

Esto imprimirá el mensaje "¡Error! Algo salió mal." en la salida de error estándar. También se puede utilizar la función `writeln!` para escribir a la salida de error estándar con formato. Por ejemplo:

```rust
let n = 10;
let m = 5;

writeln!(std::io::stderr(), "La suma de {} y {} es {}", n, m, n+m);
```

Esto imprimirá el mensaje "La suma de 10 y 5 es 15" en la salida de error estándar.

## Profundizando

Cuando se utiliza la macro `eprintln!`, el mensaje se imprime en la salida de error estándar sin ningún procesamiento adicional. Sin embargo, con la función `writeln!` se puede especificar dónde se desea imprimir el mensaje de error, lo que puede ser útil en situaciones específicas. Por ejemplo, se puede utilizar `std::io::stderr()` para imprimir en la salida de error estándar, o `std::fs::File::create("error_log.txt").unwrap()` para guardar el mensaje de error en un archivo.

Además, se pueden utilizar diferentes tipos de formato en el mensaje que se desea imprimir, como `%d` para números enteros, `%f` para números de punto flotante, `%s` para cadenas, entre otros. Esto permite personalizar el mensaje de error y hacerlo más claro y comprensible para el usuario.

## Ver también

- [Documentación de Rust sobre la macro eprintln](https://doc.rust-lang.org/std/macro.eprintln.html)
- [Documentación de Rust sobre la función writeln](https://doc.rust-lang.org/std/macro.eprintln.html)
- [Artículo sobre escritura a la salida de error estándar en Rust](https://www.deadcoderising.com/2017-12-12-rust-tutorial-writing-to-standard-error/)