---
title:                "Escribiendo en el error estándar"
date:                  2024-01-19
html_title:           "Arduino: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qué y por qué?
Escribir en el error estándar significa mandar mensajes de error a un canal específico, distinto al de la salida estándar. Los programadores hacen esto para separar los errores normales de la salida de programas y facilitar la depuración.

## Cómo hacerlo:
```Rust
use std::io::{self, Write};

fn main() {
    writeln!(io::stderr(), "¡Error! Algo salió mal.").expect("Error al escribir en stderr");
}
```

Salida de muestra (en consola):
```
¡Error! Algo salió mal.
```

## Profundización
Históricamente, separar la salida estándar (`stdout`) del error estándar (`stderr`) permitió a los usuarios redireccionarlos independientemente, lo cual es crucial para la automatización y el registro en sistemas Unix. Alternativamente, podrías usar librerías como `log` para más funciones de registro. Internamente, `stderr` es un canal no almacenado en búfer, asegurando que los mensajes de error se muestren de inmediato.

## Ver también
- Documentación oficial de std::io en Rust: https://doc.rust-lang.org/std/io/
- Tutorial de Rust sobre manejo de errores: https://doc.rust-lang.org/book/ch09-00-error-handling.html
- Guía de la librería `log`: https://docs.rs/log/latest/log/
