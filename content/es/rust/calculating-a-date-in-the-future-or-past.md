---
title:                "Calcular una fecha en el futuro o pasado"
html_title:           "Rust: Calcular una fecha en el futuro o pasado"
simple_title:         "Calcular una fecha en el futuro o pasado"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Calcular una fecha en el futuro o pasado es una operación común en programación donde se cambia la fecha actual por una fecha previa o futura. Los programadores lo hacen para resolver distintas tareas, como establecer temporizadores, agendar eventos o calcular la diferencia en días entre dos fechas.

## Cómo:

En Rust, usamos el paquete de `chrono` para calcular una fecha en el futuro o pasado. Instálalo agregándolo a tu archivo `Cargo.toml` en la sección de dependencias:

```Rust
[dependencies]
chrono = "0.4"
```

Para calcular una fecha futura, podemos utilizar el método `add`:

```Rust
use chrono::{Duration, Utc};

fn main() {
	let hoy = Utc::now();
	let un_mes_después = hoy + Duration::days(30);

	println!("Fecha actual: {}", hoy);
	println!("Un mes después: {}", un_mes_después);
}
```

Si haces run, la salida puede ser similar a:

```Rust
Fecha actual: 2021-08-23 14:20:06.807218 UTC
Un mes después: 2021-09-22 14:20:06.807218 UTC
```

Para calcular una fecha pasada, puedes utilizar el método `sub`:

```Rust
use chrono::{Duration, Utc};

fn main() {
	let hoy = Utc::now();
	let un_mes_antes= hoy - Duration::days(30);

	println!("Fecha actual: {}", hoy);
	println!("Un mes antes: {}", un_mes_antes);
}
```

En ejecución, la salida podría ser:

```Rust
Fecha actual: 2021-08-23 14:20:06.807218 UTC
Un mes antes: 2021-07-24 14:20:06.807218 UTC
```

## Deep Dive:

Históricamente, los programadores han usado muchas maneras para calcular fechas en el pasado o futuro, algunas de las cuales eran propensas a errores. Rust ofrece un enfoque seguro, gracias al paquete `chrono`. Alternativas a `chrono` pueden ser el paquete `time` o incluso el módulo estándar `std::time`, aunque `chrono` es más completo y amigable para el desarrollador. Es importante notar que `Duration::days(30)` asume un mes como 30 días, lo cual no es siempre exacto. Tal suposición puede ser suficiente en muchas aplicaciones, pero si necesitas mayor precisión, tendrías que manejar los meses de manera más detallada.

## Ver También:

* Chrono en [crates.io](https://crates.io/crates/chrono)
* Documentación oficial de Chrono [aquí](https://docs.rs/chrono/)
* Rústica [Guía de Programación del Tiempo](https://rust-lang-nursery.github.io/rust-cookbook/datetime/duration.html).