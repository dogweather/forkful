---
title:                "Escribiendo pruebas"
date:                  2024-01-19
simple_title:         "Escribiendo pruebas"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Escribir pruebas es crear código para verificar que otro código funcione correctamente. Los programadores las usan para atrapar errores, evitar regresiones y garantizar que el comportamiento del código se mantenga tras cambios y adiciones futuras.

## Cómo hacerlo:

En Rust, escribirás pruebas unitarias en el mismo archivo de tu código usando el atributo `#[test]`. Aquí tienes un ejemplo simple de una función y su prueba:

```Rust
fn suma(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prueba_suma() {
        assert_eq!(suma(2, 2), 4);
    }
}
```

Si corres las pruebas con `cargo test`, deberías ver algo así:

```
   Compiling mi_caja v0.1.0 (/path/to/mi_caja)
    Finished test [unoptimized + debuginfo] target(s) in 0.31s
     Running unittests src/lib.rs (target/debug/deps/mi_caja-abc123)

running 1 test
test tests::prueba_suma ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s
```

## Análisis Profundo:

Las pruebas en Rust tienen sus raíces en prácticas de desarrollo de software como TDD (Test-Driven Development). Aunque hay alternativas como las pruebas de integración, que se guardan en la carpeta `/tests`, las pruebas unitarias son esenciales y están integradas en la herramienta de construcción Cargo de Rust. Utilizan aserciones (`assert!`, `assert_eq!`, `assert_ne!`) para verificar condiciones y resultados esperados.

## Ver También:

- La guía oficial de Rust sobre pruebas: [Rust Book - Testing](https://doc.rust-lang.org/book/ch11-00-testing.html)
- Documentación de Cargo con respecto a las pruebas: [Cargo Guide - Tests](https://doc.rust-lang.org/cargo/guide/tests.html)
