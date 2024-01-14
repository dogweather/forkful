---
title:    "Rust: Comparando dos fechas"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cómo los programas comparan fechas? La comparación de fechas es una tarea común en la programación y puede ser útil para filtrar datos, ordenar listas o verificar la validez de una entrada de usuario. En este artículo, aprenderemos cómo comparar dos fechas en Rust y por qué es importante conocer esta habilidad.

## Cómo hacerlo

Para comparar dos fechas en Rust, podemos utilizar el tipo de datos `DateTime` de la biblioteca `chrono`. Primero, importaremos la biblioteca en nuestro código:

```Rust
use chrono::prelude::*;
```

Luego, podemos crear dos variables con fechas diferentes y utilizar el método `.cmp()` para compararlas. Por ejemplo, para verificar si la fecha "2021-01-01" es anterior a la fecha "2021-01-15", podemos escribir lo siguiente:

```Rust
let fecha1 = Utc.ymd(2021, 1, 1);
let fecha2 = Utc.ymd(2021, 1, 15);

if fecha1.cmp(&fecha2) == Ordering::Less {
    println!("La fecha 1 es anterior a la fecha 2");
}
```

En este ejemplo, utilizamos el tipo de datos `Utc` para crear las fechas, pero también podemos utilizar `Local` para tener en cuenta la zona horaria local.

También podemos utilizar los métodos `.lt()` (menor que), `.gt()` (mayor que), `.eq()` (igual a) para comparar las fechas. Estos métodos devolverán un valor booleano (`true` o `false`) según sea el caso.

## Profundizando

Es importante tener en cuenta que no solo podemos comparar fechas enteras, sino también partes individuales como el año, mes o día utilizando los métodos `year()`, `month()` y `day()`. Además, también podemos comparar fechas con precisión aún mayor utilizando el tipo de datos `DateTime` en lugar de `Date`, que incluye información sobre la hora exacta además de la fecha.

También debemos tener cuidado con la zona horaria al comparar fechas. Si no especificamos explícitamente la zona horaria en nuestras fechas, pueden ocurrir resultados inesperados debido a la conversión entre zonas horarias.

En caso de necesitar comparar fechas complejas, como comparar fechas con diferentes calendarios (gregoriano, islámico, etc.), existen bibliotecas de Rust que pueden ayudarnos a manejar estas tareas de manera más precisa.

## Ver también

Ahora que sabemos cómo comparar fechas en Rust, podemos continuar explorando más funciones y métodos para trabajar con fechas con las siguientes referencias:

- [Comparación de fechas en Rust de la documentación oficial de std](https://doc.rust-lang.org/std/cmp/trait.Ord.html#implementors-of-Ord)
- [Manipulación de fechas con chrono en Rust](https://crates.io/crates/chrono)
- [Tipo de datos DateTime de Rust](https://docs.rs/chrono/0.4.19/chrono/struct.DateTime.html)