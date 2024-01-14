---
title:    "Rust: Calculando una fecha en el futuro o pasado"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por qué

Calcular fechas en el futuro o en el pasado es una tarea común en la programación y puede ser útil en una variedad de situaciones. Ya sea para planificar y programar tareas o para realizar cálculos en aplicaciones financieras, entender cómo calcular fechas puede ser una habilidad valiosa para cualquier programador.

## Cómo hacerlo

¡En Rust, calcular fechas es bastante sencillo! Primero, debemos importar el módulo estándar `chrono`, que proporciona funcionalidad para trabajar con fechas y tiempos. Luego, podemos utilizar el tipo de datos `DateTime` para crear una instancia que represente una fecha específica, especificando los valores para el año, mes, día, hora, minuto y segundo. Por ejemplo:

```Rust
use chrono::{DateTime, Utc};

fn main() {
    let fecha = DateTime::parse_from_str("2020-12-25 09:00:00", "%Y-%m-%d %H:%M:%S").unwrap();

    println!("La fecha es {}", fecha.to_string());
}
```

La salida de este código será:

> La fecha es 2020-12-25 09:00:00

También podemos realizar cálculos con fechas utilizando métodos proporcionados por el tipo `DateTime`. Por ejemplo, si queremos obtener la fecha 7 días a partir de hoy, podemos usar el método `add(chrono::Duration)` para agregar una cantidad específica de días a nuestra fecha actual. Veamos un ejemplo:

```Rust
use chrono::{DateTime, Duration, Utc};

fn main() {
    let hoy = Utc::now();
    let fecha_futura = hoy.add(Duration::days(7));

    println!("Hoy es {}", hoy.to_string());
    println!("Fecha en 7 días será {}", fecha_futura.to_string());
}
```

La salida de este código será:

> Hoy es 2021-07-10 14:35:00
> Fecha en 7 días será 2021-07-17 14:35:00

## Profundizando

Para aquellos interesados en entender más a fondo cómo funciona la manipulación de fechas en Rust, es importante tener en cuenta que el tipo de datos `DateTime` es inmutable, lo que significa que no podemos modificar sus valores directamente. En su lugar, debemos usar métodos como `add` y `subtract` para realizar cálculos o manipular fechas.

Además, es importante tener en cuenta que, al igual que en la mayoría de los lenguajes de programación, las fechas y horas se almacenan internamente como valores numéricos. En Rust, se utilizan timestamps basados en la época Unix (1 de enero de 1970), que representan el número de segundos transcurridos desde esa fecha.

## Ver también

- Microsoft Developer: [Trabajar con fechas y horas en Rust](https://docs.microsoft.com/es-es/learn/modules/rust-datetime/)
- Rust Cookbook: [Manipulando fechas y horas con Rust](https://rust-lang-nursery.github.io/rust-cookbook/datetime.html#manipulating-datetime)
- Chrono crate: [Documentación oficial](https://docs.rs/chrono/0.4.19/chrono/)