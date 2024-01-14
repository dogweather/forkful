---
title:    "Rust: Imprimir salida de depuración"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

Imprimir resultados de depuración (debug output) es una herramienta vital en la programación en Rust. Ayuda a los desarrolladores a identificar errores, resolver problemas y entender el flujo del programa.

## Cómo hacerlo

Para imprimir resultados de depuración en un programa Rust, podemos utilizar la macro `println!()`. Esta macro toma una cadena de texto como argumento y puede imprimir cualquier valor que se le pase.

```
Rust
fn main() {
  let numero = 42;
  println!("El número es: {}", numero);
}
```

La salida de este programa sería `El número es: 42`.

La macro `println!()` también acepta múltiples argumentos separados por comas y podemos utilizar el formato de impresión para personalizar la salida. Por ejemplo:

```
Rust
fn main() {
  let nombre = "Juan";
  let edad = 29;
  let altura = 1.80;

  println!("¡Hola {}, tienes {} años y mides {:.2} metros!", nombre, edad, altura);
}
```

La salida sería `¡Hola Juan, tienes 29 años y mides 1.80 metros!`.

## Profundizando

Además de `println!()`, también podemos utilizar la macro `eprintln!()` para imprimir errores en la consola. Esta macro funciona de la misma manera que `println!()`, pero la salida se imprime en la salida de errores en lugar de la salida estándar.

Otra forma de imprimir resultados de depuración es utilizando la función `dbg!()`. Esta función toma un valor como argumento y lo imprime junto con su nombre y ubicación en el código. Por ejemplo:

```
Rust
fn main() {
  let numero = 42;
  dbg!(numero);
}
```

La salida sería `(src/main.rs:3) numero = 42`.

## Ver también

- [La documentación oficial de Rust sobre macros de impresión](https://doc.rust-lang.org/std/macro.println.html)
- [Un tutorial de Impresión de Depuración en Rust](https://www.educative.io/blog/rust-debugging)
- [Una guía para usar la macro `dbg!()` en Rust](https://www.baeldung.com/rust-console-printing)