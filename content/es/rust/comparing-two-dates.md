---
title:    "Rust: Comparando dos fechas"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué comparar dos fechas en Rust?

Comparar dos fechas puede ser una tarea común en muchas aplicaciones, ya sea para mostrar información ordenada por fechas o para realizar cálculos de tiempo transcurrido. En Rust, hay varias formas de comparar fechas y en este artículo exploraremos cómo hacerlo de manera eficiente.

## Cómo hacerlo

En Rust, las fechas son representadas por el tipo de datos `DateTime<Utc>` del paquete `chrono`. Para comparar dos fechas, podemos utilizar el método `.cmp()` que devuelve un valor `Ordering` indicando si la primera fecha es menor, igual o mayor que la segunda. Veamos un ejemplo:

```Rust
use chrono::{Utc, DateTime};
use std::cmp::Ordering;

let date_1: DateTime<Utc> = Utc.ymd(2021, 8, 20).and_hms(12, 0, 0);
let date_2: DateTime<Utc> = Utc.ymd(2021, 8, 15).and_hms(15, 0, 0);

let result = date_1.cmp(&date_2);

match result {
    Ordering::Less => println!("La fecha 2 es mayor a la fecha 1"),
    Ordering::Equal => println!("Las fechas son iguales"),
    Ordering::Greater => println!("La fecha 1 es mayor a la fecha 2"),
}
```

En este ejemplo, primero importamos el paquete `chrono` y el módulo `Ordering` de la librería estándar para poder utilizar el método `.cmp()`. Luego, creamos dos fechas utilizando el método `Utc.ymd()` y `Utc.and_hms()` para especificar el año, mes, día y hora. Finalmente, comparamos las fechas usando `.cmp()` y utilizamos un `match` para imprimir un mensaje dependiendo del resultado.

Además del método `.cmp()`, también podemos utilizar los operadores de comparación (`<`, `>`, `==`, `!=`) para comparar fechas en Rust. Sin embargo, es importante tener en cuenta que esto solo compara la fecha y hora exactas, no toma en cuenta la zona horaria ni otros aspectos específicos de la fecha.

## Deep Dive

En la comparación de fechas, también es importante tener en cuenta el concepto de precisión. Las fechas pueden tener diferentes niveles de precisión, desde milisegundos hasta años completos. Rust nos permite especificar el nivel de precisión al crear una fecha, lo que puede afectar cómo se comparan las fechas entre sí.

Por ejemplo, si tenemos dos fechas con diferente nivel de precisión, como una con segundos y otra solo con minutos, pueden ser consideradas iguales por Rust, a pesar de no ser idénticas. Por eso, es importante tener en cuenta la precisión al comparar fechas en Rust.

## Ver también
- [Documentación del paquete `chrono`](https://docs.rs/chrono/0.4.19/chrono/)
- [Pautas de formato de fechas y horas en Rust](https://doc.rust-lang.org/std/fmt/trait.Display.html#method.to_string)
- [Ejemplos de uso de fechas en Rust](https://github.com/chrono-rs/chrono-rs/tree/master/examples)