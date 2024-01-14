---
title:    "Rust: Calcular una fecha en el futuro o en el pasado."
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

Calcular fechas en el futuro o pasado puede ser útil en muchas situaciones, como planificar eventos, tareas o recordatorios. En este blog post, aprenderemos cómo hacerlo en Rust.

## Cómo hacerlo

Para calcular una fecha futura o pasada en Rust, necesitamos usar la librería `chrono`. Primero, importamos la librería en nuestro código:

```Rust
use chrono::{DateTime, Utc, Duration};
```

Luego, creamos un objeto `DateTime` con la fecha actual en UTC:

```Rust
let date = Utc::now();
```

Para calcular una fecha en el futuro, utilizamos el método `checked_add` junto con el objeto `Duration`. Por ejemplo, si queremos sumar 2 días a la fecha actual, haríamos lo siguiente:

```Rust
let future_date = date.checked_add(Duration::days(2));
```

Para calcular una fecha en el pasado, utilizamos el método `checked_sub` en lugar de `checked_add`. Por ejemplo, si queremos restar 5 días a la fecha actual, haríamos lo siguiente:

```Rust
let past_date = date.checked_sub(Duration::days(5));
```

Podemos cambiar la unidad de tiempo en el método `Duration` según nuestras necesidades, como horas, minutos o segundos.

## Profundizando

En la librería `chrono`, también podemos especificar una zona horaria diferente a UTC y trabajar con fechas y horas en un formato específico. Además, podemos realizar operaciones más avanzadas, como comparar fechas o calcular la diferencia en tiempo entre dos fechas.

Para obtener más información sobre la librería `chrono`, puedes consultar su documentación en el siguiente enlace: https://docs.rs/chrono/.

## Ver también

- Librería `chrono`: https://docs.rs/chrono/
- Tutorial sobre la librería `chrono` en Rust: https://www.youtube.com/watch?v=wcY2dAoheqQ