---
title:                "Rust: Imprimir salida de depuración"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué
Imprimir el resultado de depuración (debug output) puede ser una herramienta muy útil para identificar errores y entender cómo funciona nuestro código en Rust.

## Cómo hacerlo
Para imprimir mensajes de depuración en Rust, podemos utilizar la macro `println!()`. Esta macro funciona de manera similar a la función `println()` en otros lenguajes de programación, pero con algunas diferencias. Veamos un ejemplo:

```Rust
fn main() {
    let numero = 10;
    println!("El número es: {}", numero);
}
```

En este ejemplo, utilizamos la macro `println!()` para imprimir un mensaje junto con el valor de la variable `numero`. Notarás que dentro de las llaves `{}`, colocamos el símbolo `#` seguido del nombre de la variable. Esto indica que queremos imprimir el valor de esa variable en ese lugar dentro del mensaje.

También podemos utilizar la macro `eprintln!()` para imprimir mensajes de error en lugar de la macro `println!()`.

```Rust
fn main() {
    let edad = 25;
    eprintln!("La edad debe ser mayor a 18 años");
    println!("La persona tiene {} años", edad);
}
```

En este segundo ejemplo, utilizamos la macro `eprintln!()` para imprimir un mensaje de error, seguido de la macro `println!()` para imprimir el valor de la variable `edad`. 

## Profundizando
Además de utilizar la macro `println!()` y `eprintln!()`, también podemos utilizar la macro `dbg!()` para imprimir mensajes de depuración. Esta macro nos permite imprimir tanto el mensaje como el valor de una variable en una sola línea.

```Rust
fn main() {
    let nombre = "Jennifer";
    let apellido = "Lopez";
    let edad = 51;
    dbg!(nombre, apellido, edad);
}
```

Al ejecutar este código, obtendremos el siguiente resultado:

```
[src/main.rs:5] nombre = "Jennifer"
[src/main.rs:6] apellido = "Lopez"
[src/main.rs:7] edad = 51
```

Podemos utilizar la macro `dbg!()` en cualquier lugar donde queramos imprimir valores de depuración, incluso dentro de una expresión.

```Rust
fn main() {
    let numero = 10;
    let doble = dbg!(numero * 2);
}
```

El resultado de esta expresión sería `src/main.rs:3 numero * 2 = 20`, lo que nos permite ver rápidamente el resultado de nuestra operación de manera más clara.

## Ver también
- [Documentación oficial de Rust sobre la macro `println!()`](https://doc.rust-lang.org/std/macro.println.html)
- [Tutorial sobre la macro `println!()` en Rust](https://www.educative.io/edpresso/the-println-macro-in-rust)
- [Guía completa de Rust para principiantes](https://dev.to/kristencodes/a-complete-guide-to-writing-structs-in-rust-3k1a)