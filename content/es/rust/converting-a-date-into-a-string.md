---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "C++: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Porqué?
La creación de una cadena de texto desde una fecha -abostracción a semejante proceso- permite a los programadores mostrar y manipular la fecha en un formato legible. Esto es especialmente útil en interfaz de usuario y operaciones de base de datos.

## ¿Cómo hacerlo?
Ahora, muy brevemente, veamos cómo puedes convertir la fecha en una cadena en Rust.

```Rust
use chrono::{DateTime, Utc};
let now: DateTime<Utc> = Utc::now();
let fecha_string = now.to_rfc3339();
println!("Fecha y Hora : {}", fecha_string);
```

Cuando ejecutas este código, la salida será similar a esta:

```Rust
Fecha y Hora : 2020-08-31T12:30:00Z
```

## Profundización
Primero, un poco de historia. El sistema de fechas y tiempos de Rust está inspirado en la biblioteca de fechas y tiempos de Joda. Y el método `to_rfc3339` que utilizamos aquí es un estándar en el diseño de Internet rastreado hasta 1996.

Hay alternativas para convertir una fecha en una cadena. Puedes usar la función `format!` en lugar de `to_rfc3339`, lo que te permitirá más libertad para especificar el formato de la cadena de la fecha.

``` Rust
let fecha_string = format!("{}", now.format("%Y-%m-%d %H:%M:%S"));
```

La salida será la misma que antes.

Implementar fechas en cadenas se trata sobre la abstracción de características internas a formatos dispuestos para operaciones más complejas de manipulación de delación y visualización.

## Ver También
Para continuar aprendiendo aún más, aquí hay algunos enlaces útiles:

- Documentación oficial de Rust sobre el módulo estándar de tiempo: [https://doc.rust-lang.org/std/time/](https://doc.rust-lang.org/std/time/)
  
- Crates de la biblioteca Chrono: [https://docs.rs/chrono/0.4.7/chrono/](https://docs.rs/chrono/0.4.7/chrono/)
  
- Detalles sobre el estándar RFC 3339: [https://tools.ietf.org/html/rfc3339](https://tools.ietf.org/html/rfc3339)