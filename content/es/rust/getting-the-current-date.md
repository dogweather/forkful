---
title:                "Obteniendo la fecha actual"
html_title:           "Rust: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Obtener la fecha actual es una tarea común en la programación, ya que a menudo es necesario para tareas como registro de eventos, generación de informes y seguimiento del tiempo. Los programadores utilizan funciones y métodos para obtener la fecha actual en su código.

## Cómo:

```Rust
use std::time::SystemTime;

let now = SystemTime::now(); // Obtiene la fecha actual
```

Este código utiliza el módulo `std::time` y el método `now()` para obtener la fecha y hora actual en la que se ejecuta el código.

```Rust
use chrono::prelude::*;

let now: DateTime<Utc> = Utc::now(); // Obtiene la fecha y la hora actual en UTC
```

Este ejemplo utiliza la biblioteca externa `chrono` para obtener la fecha y hora actuales en formato UTC.

## Profundizando

En el pasado, obtener la fecha actual requería cálculos complejos y variaba según el sistema operativo. Ahora, en Rust, se pueden usar métodos sencillos como `now()` para obtener la fecha actual sin preocuparse por el sistema operativo subyacente.

Como alternativa, los programadores también pueden utilizar bibliotecas externas como `chrono` o `time` para obtener la fecha y hora actual en formatos específicos o realizar cálculos con la fecha actual.

Internamente, Rust utiliza el reloj del sistema para obtener la fecha actual, que puede variar según la configuración del sistema operativo. Al utilizar bibliotecas externas, se pueden realizar cálculos para ajustar la fecha y hora según las necesidades del programa.

## Véase también

- [La documentación oficial de Rust](https://doc.rust-lang.org/std/time/struct.SystemTime.html)
- [La biblioteca externa `chrono`](https://docs.rs/chrono/0.4.10/chrono/)
- [La biblioteca externa `time`](https://docs.rs/time/0.2.17/time/)