---
title:                "Comparando dos fechas"
html_title:           "C#: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Comparar dos fechas implica determinar cuál es anterior o posterior, o si ambas son iguales. Los programadores hacen esto para tomar decisiones basadas en el tiempo, por ejemplo, programar eventos o validar períodos de suscripción.

## Cómo:

Aquí te muestro cómo puedes hacerlo en Rust. Tenemos `SystemTime` en la biblioteca estándar, que representa un punto en el tiempo.

```Rust
use std::time::SystemTime;

let ahora = SystemTime::now();
let despues = SystemTime::now();

println!("{:?}", ahora < despues);
```

Resultado:

```Rust
false
```

Como puedes ver, la comparación directa nos dice si la fecha `despues` es posterior a `ahora`.

## Inmersión Profunda

Históricamente, las fechas y horas se han comparado contando los segundos desde un punto específico en el tiempo (la "época"). Sin embargo, esto puede dar lugar a errores debido a anomalías como el tiempo de verano y los segundos intercalares.

Una alternativa es usar bibliotecas de terceros como `chrono`, que ofrecen una mayor precisión y características adicionales.

Los detalles de implementación en Rust son bastante directos gracias a la sobrecarga del operador. Bajo el capó, `SystemTime::now()` devuelve el tiempo transcurrido desde la época, y la comparación se realiza verificando este valor.

## Ver También

Para una mayor profundidad sobre el manejo del tiempo y la comparación de fechas puedes consultar las siguientes fuentes:

- [Biblioteca estándar de Rust - SystemTime](https://doc.rust-lang.org/std/time/struct.SystemTime.html)
- [Biblioteca Chrono](https://docs.rs/chrono/0.4.19/chrono/)