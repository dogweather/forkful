---
title:                "Usando un depurador"
aliases: - /es/rust/using-a-debugger.md
date:                  2024-01-26T04:10:00.232015-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando un depurador"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/using-a-debugger.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Usar un depurador es como otorgarte una visión de rayos X para echar un vistazo a la ejecución de tu código. Los programadores lo hacen para detectar errores, entender el flujo del programa y asegurarse de que su código esté limpio como un silbato. Es como tener un amigo que señala exactamente dónde te has tropezado.

## Cómo hacerlo:

Rust soporta varios depuradores, pero uno común es `gdb` para GNU/Linux o `lldb` para macOS. También podrías usar `rust-gdb` o `rust-lldb`, que son envoltorios que imprimen de forma más legible los valores de Rust. Aquí tienes un vistazo:

```Rust
fn main() {
    let mut contador = 0;
    for _ in 0..5 {
        contador += 1;
        println!("El contador está en: {}", contador);
    }
}
```

Para depurar esto, compila con información de depuración:

```shell
$ rustc -g contador.rs
```

Luego ejecútalo en `rust-gdb`:

```shell
$ rust-gdb contador
(gdb) break main
(gdb) run
(gdb) print contador
$1 = 0
(gdb) continue
El contador está en: 1
(gdb) print contador
$2 = 1
```

## Profundización

La depuración ha existido desde los *tiempos antiguos* de las tarjetas perforadas, y su evolución ha sido una bendición. Rust proporciona sus propias herramientas con integraciones para GDB y LLDB debido a la naturaleza a nivel de sistema del lenguaje.

Las alternativas para depurar código Rust incluyen el uso de entornos de desarrollo integrados (IDEs) con sus depuradores integrados, los cuales algunos encuentran más intuitivos. Los populares incluyen CLion con el complemento de Rust o Visual Studio Code con la extensión de Rust.

En cuanto a la implementación, Rust genera símbolos de depuración que estos depuradores entienden, lo cual es vital para avanzar paso a paso por el código, establecer puntos de interrupción e inspeccionar variables sin perder la cabeza.

## Ver También

- El Libro de Rust sobre Depuración: https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#guidelines-for-error-handling
- La perspectiva de Rust Por Ejemplo sobre Errores y Depuración: https://doc.rust-lang.org/rust-by-example/error.html
- El Servidor de Lenguaje de Rust (RLS) que potencia la extensión de Rust de VS Code: https://github.com/rust-lang/rls
- Depurando Rust con Visual Studio Code: https://marketplace.visualstudio.com/items?itemName=rust-lang.rust
