---
title:    "Rust: Obteniendo la fecha actual"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por qué

En la programación, es común la necesidad de obtener la fecha y hora actuales. Esta información es útil para llevar un registro del tiempo de ejecución de un programa o para mostrar la hora actual a los usuarios. En este blog post, aprenderemos cómo obtener la fecha actual en Rust.

## Cómo hacerlo

Para obtener la fecha y hora actual en Rust, podemos utilizar el módulo `chrono`, el cual nos permite trabajar con fechas y tiempos de una manera sencilla y eficiente. Primero, debemos agregar `chrono` a nuestro archivo `Cargo.toml`:

```Rust
[dependencies]
chrono = "0.4"
```

Una vez agregado el módulo, podemos utilizarlo en nuestro código de la siguiente manera:

```Rust
use chrono::prelude::*;
```

Esta línea de código nos permite acceder a todas las funcionalidades de `chrono`. Ahora podemos crear una variable que almacene la fecha y hora actual:

```Rust
let current_date = Local::now();
```

El método `now()` nos retorna la fecha y hora local actual en formato `DateTime<Local>`. Si queremos obtener solo la fecha o la hora, podemos utilizar los métodos `date()` y `time()` respectivamente.

```Rust
let current_date = Local::now().date();
let current_time = Local::now().time();
```

También podemos formatear la fecha y hora de acuerdo a nuestras necesidades utilizando el método `format()` y especificando el formato deseado.

```Rust
let current_date = Local::now();
let formatted_date = current_date.format("El día %d de %B de %Y");
```

En este caso, hemos especificado un formato en español, donde `%d` representa el día, `%B` el mes y `%Y` el año.

## Profundizando

`chrono` nos ofrece una gran variedad de métodos para trabajar con fechas y tiempos. Podemos utilizar el tipo de dato `DateTime<T>` para trabajar con fechas en diferentes timezones, o utilizar `Duration` para realizar operaciones de suma o resta de tiempo. Sin embargo, es importante recordar que la fecha y hora actual puede variar dependiendo de la zona horaria en la que se encuentre el usuario, por lo que es recomendable utilizar `Local` para obtener la hora y fecha local.

## Ver también

- [Documentación de chrono](https://docs.rs/chrono/0.4.19/chrono/)
- [Ejemplo de uso de chrono](https://github.com/chronotope/chrono/tree/0.4#examples)
- [Tutorial de Rust en español](https://www.rust-lang.org/es-ES/learn/get-started)