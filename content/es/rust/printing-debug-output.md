---
title:                "Rust: Imprimiendo resultados de depuración"
simple_title:         "Imprimiendo resultados de depuración"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

Una de las tareas más comunes en la programación es la impresión de mensajes de depuración. Esto puede ayudarnos a entender mejor cómo se está ejecutando nuestro código y a detectar posibles errores. En Rust, hay varias formas de imprimir mensajes de depuración, y en esta publicación vamos a explorarlas juntas.

## Cómo hacerlo

Para imprimir un mensaje de depuración en Rust, podemos utilizar la macro `dbg!()`. Esta macro toma una expresión como argumento y la imprime junto con su valor. Por ejemplo:

```Rust
let x = 5;
dbg!(x); // Imprime "x: 5"
```

También podemos utilizar la macro `println!()` para imprimir mensajes de depuración. Esta macro es muy flexible, ya que nos permite incluir variables y formatear la salida. Por ejemplo:

```Rust
let name = "Luisa";
println!("El nombre es {}", name); // Imprime "El nombre es Luisa"
```

Otra opción es utilizar el logger integrado de Rust, `log`. Este logger es más complejo de configurar, pero nos permite tener un mayor control sobre nuestros mensajes de depuración.

## Profundizando

Ahora que hemos visto cómo imprimir mensajes de depuración en Rust, es importante mencionar que podemos desactivar estas impresiones en las compilaciones de producción. Esto se logra utilizando la directiva `#[cfg(debug_assertions)]`, que nos permite compilar el código sólo cuando la opción de depuración está habilitada.

También podemos pasar variables a nuestras macros de depuración utilizando el operador `?`.

Por último, si queremos imprimir un mensaje de depuración solo en ciertas condiciones, podemos utilizar la macro `debug_assert!()`. Esta macro nos permite imprimir mensajes de depuración sólo cuando se cumple una condición específica.

## Ver también

- [Documentación oficial de Rust sobre impresión de mensajes de depuración.](https://doc.rust-lang.org/std/macro.dbg.html)
- [Tutorial sobre cómo utilizar el logger integrado de Rust.](https://blog.logrocket.com/getting-started-with-the-rust-logging-framework/)
- [Artículo sobre cómo utilizar la directiva de compilación `#[cfg(debug_assertions)]`.](https://doc.rust-lang.org/reference/conditional-compilation.html)