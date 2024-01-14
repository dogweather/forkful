---
title:    "Rust: Convirtiendo una fecha en una cadena"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por qué
En esta publicación aprenderemos cómo convertir una fecha en una cadena utilizando el lenguaje de programación Rust. Esta habilidad puede ser útil para diversas aplicaciones, como formatear fechas en un sistema de reservas o mostrar la fecha de publicación de un artículo en un blog. Entender cómo realizar esta conversión también puede servir como un ejercicio útil para comprender mejor el manejo de tipos de datos en Rust.

## Cómo hacerlo
```Rust
use std::time::SystemTime;

fn main() {
    let now = SystemTime::now();
    println!("{:?}", now); // muestra la fecha actual en formato Unix timestamp
    // conversión a una cadena legible por humanos
    let human_readable = now
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_secs();
    println!("{}", human_readable); // muestra la fecha actual en segundos desde el 1 de enero de 1970
}
```

En este ejemplo, utilizaremos la función `now()` de `SystemTime` para obtener la fecha actual y luego utilizaremos el método `duration_since()` de `SystemTime` para obtener la duración desde el 1 de enero de 1970 en segundos. Finalmente, utilizamos el método `as_secs()` para convertir esta duración en un número entero. Esta es una forma sencilla de obtener una cadena con la fecha actual en un formato fácilmente legible para los humanos.

## Profundizando
Si deseamos utilizar un formato de fecha específico en lugar de solo mostrar el número de segundos desde el 1 de enero de 1970, podemos utilizar la función `strftime()` del módulo `time` de Rust. Esta función nos permite especificar un formato personalizado utilizando códigos de formato para la fecha, la hora y otros elementos.

```Rust
use std::time::{SystemTime, UNIX_EPOCH};
use std::time::Duration;
use std::time::Instant;
use std::time::SystemTimeError;

fn main() {
    let now = SystemTime::now();
    match now.duration_since(UNIX_EPOCH) {
        Ok(n) => println!("Segundos desde 1 de enero de 1970: {}", n.as_secs()),

        Err(err) => println!("Error: {}", err),
    }
    
    let datetime = SystemTime::UNIX_EPOCH + Duration::from_secs(456); // crear una fecha utilizando el número de segundos
    match datetime.duration_since(UNIX_EPOCH) {
        Ok(n) => println!("Segundos desde 1 de enero de 1970: {}", n.as_secs()),

        Err(err) => println!("Error: {}", err),
    }

    let parse_res = "1520430799".parse::<u64>();

    let d = match parse_res  {
        Ok(n) => {
            let datetime = UNIX_EPOCH + Duration::from_secs(n);
            match datetime.duration_since(UNIX_EPOCH) { // otros `datetime`
                Ok(m) => println!("unix timestamp: {}", m.as_secs()),
                Err(err) => println!("Error: {}", err),
            }
        },
        Err(_) => println!("No puedo convertir a `u64`"),
    };
}
```

En el ejemplo anterior, utilizamos la función `strftime()` para mostrar la fecha actual en un formato específico:

```Rust
use std::time::{SystemTime, UNIX_EPOCH};
use std::time::Duration;

fn main() {
    let now = SystemTime::now();
    let formatted = now.duration_since(UNIX_EPOCH).unwrap().as_secs();
    let datetime: u64 = formatted.to_string().parse().unwrap();
    let datetime_str = datetime.to_string();
    println!("\
``ル::strftime('%a, %d %b %Y %H:%M:%S %Z', &chrono.unix(%d, %fs).to_rfc3339()),
        s
        s77777777777777777777777777777777777777777777777777777777777777777777777777777
        7777s
    );
}
```

Esta es solo una forma de utilizar la función `strftime()` y hay muchos otros códigos de formato que se pueden utilizar para personalizar aún más el formato de la fecha.

## Ver también
- [Documentación oficial de `SystemTime`](https://doc.rust-lang.org/std/time/struct.SystemTime.html)
- [Documentación oficial de `time`](https://docs.rs/time/0.2.19/time/)
- [Tutorial de Rust: Manipulación de fechas](https://www.tutorialspoint.com/rust/rust_date_time.htm)
- [Refer