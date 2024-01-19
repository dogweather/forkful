---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elixir: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Borrando caracteres que coinciden con un patrón en Rust

## ¿Qué y por qué?

Eliminar caracteres que coinciden con un patrón es una operación común que implica quitar todas las instancias de un patrón específico en una cadena de texto. Los programadores hacen esto para limpiar datos, extraer información específica o transformar textos.

## ¿Cómo hacerlo?

Aquí tienes un ejemplo de cómo puedes hacer esto en Rust. La operación `replace()` viene al rescate, permitiéndonos reemplazar todos los casos de un patrón con otra cadena elegida.

```Rust
fn main() {
    let s = "Hola, mi número de teléfono es 123456. Por favor, no lo compartas.";
    let cleaned = s.replace("123456", "");
    println!("{}", cleaned);
}
```

La salida de este programa sería:

```
Hola, mi número de teléfono es . Por favor, no lo compartas.
```

Así hemos eliminado todas las instancias de "123456" de la cadena de texto.

## Análisis en profundidad

La función `replace()` en Rust no es específica para la limpieza de texto, pero se utiliza con frecuencia para este propósito. Apareció en Rust 1.0, y ha sido una herramienta valiosa para los programadores de Rust desde entonces.

Una alternativa sería usar el módulo `regex` para eliminar caracteres que coinciden con un patrón más complejo. Sin embargo, `replace()` es más rápida y fácil de utilizar para patrones sencillos.

La supresión de caracteres que coinciden con un patrón se implementa en Rust a través de un ciclo en el que el programa busca el patrón en la cadena, y si lo encuentra, lo reemplaza con la segunda cadena proporcionada, en este caso, una cadena vacía.

## Ver también

Para más información sobre el tema, puedes consultar los siguientes enlaces:
- [Documentación oficial de Rust sobre la función `replace`](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Tutorial de Rust Strings](https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html)
- [Discusión de la comunidad de Rust sobre el manejo de cadenas](https://users.rust-lang.org/t/how-to-handle-string-in-rust/26396)