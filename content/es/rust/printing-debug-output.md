---
title:    "Rust: Imprimiendo salida de depuración"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# ¿Por qué imprimir mensajes de depuración en Rust?

Imprimir mensajes de depuración es una técnica comúnmente utilizada por los programadores para ayudar a identificar y solucionar errores en su código. En Rust, esto es especialmente importante debido a su enfoque en la seguridad y la ausencia de errores. Imprimir mensajes de depuración es una forma efectiva de comprender qué está sucediendo en tu código y encontrar posibles errores.

## Cómo imprimir mensajes de depuración

Para imprimir mensajes de depuración en Rust, podemos utilizar la macro `println!`. Esta macro acepta un string como argumento y puede contener placeholders para variables que deseamos imprimir. Dentro de los placeholders, se utiliza un signo de exclamación (!) después del tipo de dato para indicar que queremos imprimir el valor en su forma depurada.

```Rust
fn main() {
    let nombre = "Juan";
    let edad = 25;
    println!("Hola, mi nombre es {} y tengo {} años", nombre, edad);
}
```
**Output:**
```
Hola, mi nombre es "Juan" y tengo 25 años
```

También podemos utilizar la macro `dbg!`, que funciona de manera similar a `println!` pero además de imprimir el valor de la variable, también imprime su nombre y tipo de dato. Esta macro es útil para imprimir múltiples variables a la vez.

```Rust
fn main() {
    let num1 = 10;
    let num2 = 20;
    let resultado = num1 + num2;
    dbg!(num1, num2, resultado);
}
```

**Output:**
```
[src/main.rs:5] num1 = 10
[src/main.rs:6] num2 = 20
[src/main.rs:7] resultado = 30
```

## Profundizando en la impresión de mensajes de depuración

Además de las macros `println!` y `dbg!`, Rust también cuenta con la macro `eprintln!` que se utiliza para imprimir mensajes de error. También es posible utilizar el operador `?` para imprimir mensajes de depuración en un contexto de error. Esto es útil para ver la cadena de errores y su causa.

Rust también ofrece una forma de controlar qué mensajes de depuración se imprimen durante la compilación. Esto se puede hacer mediante el uso del nivel de depuración `debug!` y la funcion `println!` se imprimirán:

```Rust
#[cfg(debug_assertions)]
fn main() {
    let nombre = "Juan";
    let edad = 25;
    println!("Hola, mi nombre es {} y tengo {} años", nombre, edad);
}

#[cfg(not(debug_assertions))]
fn main() {
    // codigo en producción
}
```

# Ver también

- [Documentación de Rust sobre la macro `dbg!`](https://doc.rust-lang.org/std/macro.dbg.html)
- [Artículo sobre depuración de errores en Rust](https://www.freecodecamp.org/news/how-to-debug-rust-programs/)
- [Tutorial sobre macros en Rust](https://dev.to/pedantic_programmer/macros-in-rust-1n1f)