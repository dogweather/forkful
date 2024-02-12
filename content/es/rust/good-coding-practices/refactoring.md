---
title:                "Refactorización"
aliases: - /es/rust/refactoring.md
date:                  2024-01-26T03:36:34.408385-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactorización"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/refactoring.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Refactorización es el proceso de reestructurar el código computacional existente—cambiando la factorización—sin alterar su comportamiento externo. Los programadores lo hacen para mejorar atributos no funcionales del software, como la legibilidad, reducción de complejidad, mejorar la mantenibilidad y crear una arquitectura interna o modelo de objeto más expresivo para mejorar la extensibilidad.

## Cómo hacerlo:

Vamos a refactorizar un simple fragmento de código en Rust para hacerlo más idiomático y mantenible. Empezamos con una función que calcula la suma de un vector de enteros:

```rust
fn sum(vec: &Vec<i32>) -> i32 {
    let mut sum = 0;
    for i in vec {
        sum += i;
    }
    sum
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("La suma es {}", sum(&numbers));
}
```

Salida:
```
La suma es 15
```

Ahora, vamos a refactorizar esto para usar Rust más idiomático aprovechando iteradores y el método `fold`:

```rust
fn sum(vec: &[i32]) -> i32 {
    vec.iter().fold(0, |acc, &x| acc + x)
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("La suma es {}", sum(&numbers));
}
```

No hay cambio en la salida—todavía es `15`—pero la versión refactorizada es más limpia y usa las fortalezas de Rust como el préstamo y los métodos de iterador.

## Análisis Profundo

La refactorización tiene sus raíces en la comunidad de Smalltalk y fue popularizada en el mundo de Java por el libro de Martin Fowler "Refactoring: Improving the Design of Existing Code". Sus principios son universales y aplican también a Rust, donde la seguridad y la concurrencia son primordiales. Rust fomenta escribir código robusto al capturar problemas en tiempo de compilación, así que durante la refactorización, el compilador de Rust actúa como una red de seguridad.

Las alternativas a la refactorización manual incluyen el uso de herramientas automatizadas, como 'rustfmt' para formatear el código y 'clippy' para linting, que pueden sugerir formas más idiomáticas de escribir código. Sin embargo, una refactorización profunda a menudo requiere una comprensión reflexiva del diseño del código, lo cual estas herramientas no pueden automatizar completamente.

En Rust, la refactorización puede girar en torno a mejorar el uso de tipos, aprovechando eficazmente los tiempos de vida, reduciendo las asignaciones innecesarias o empleando patrones de concurrencia como usar `Arc<Mutex<T>>` cuando sea necesario. También es común pasar de `unwrap()` a un manejo de errores más expresivo con `Result<T, E>`.

## Ver También

Para profundizar más en la refactorización en Rust:

- El Libro de Rust: https://doc.rust-lang.org/book/
- Rust por Ejemplo: https://doc.rust-lang.org/rust-by-example/
- Clippy, una herramienta de linting de Rust: https://github.com/rust-lang/rust-clippy
- "Refactoring: Improving the Design of Existing Code" por Martin Fowler: https://martinfowler.com/books/refactoring.html
