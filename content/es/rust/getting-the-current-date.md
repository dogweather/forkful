---
title:    "Rust: Obteniendo la fecha actual"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué obtener la fecha actual es importante en Rust

Rust es un lenguaje de programación moderno y eficiente que está ganando popularidad rápidamente. Una de las tareas más comunes que se realizan en la programación es obtener la fecha actual. Puede ser para mostrar la fecha en una interfaz de usuario, para realizar cálculos relacionados con el tiempo o para realizar una acción programada. En este artículo, veremos cómo obtener la fecha actual en Rust y por qué es importante para los programadores.

## Cómo obtener la fecha actual en Rust

Para obtener la fecha actual en Rust, utilizaremos el módulo `std::time` que proporciona funciones para trabajar con fechas y horas. Primero, debemos importar el módulo en nuestro código con la declaración `use std::time;`. A continuación, utilizaremos la función `now()` del módulo para obtener un `Instant` que representa el tiempo actual. Luego, podemos usar el método `format()` para dar formato a la fecha en cualquier tipo de formato que necesitemos.

```Rust
use std::time;

let now = time::Instant::now();
let date = now.format("%Y-%m-%d"); // formato año-mes-día

println!("La fecha actual es {}", date); // Salida: 2021-07-11
```

También podemos obtener la hora actual en varias zonas horarias utilizando el tipo de datos `chrono::DateTime` del popular paquete `chrono`. Este paquete proporciona una gran cantidad de funciones para trabajar con fechas y horas.

```Rust
use chrono::prelude::*;

let local_now = Local::now();
let utc_now = Utc::now();

println!("La hora actual en tu zona horaria es {}", local_now.format("%H:%M")); // Salida: 16:35
println!("La hora actual en UTC es {}", utc_now.format("%H:%M")); // Salida: 20:35
```

Hay muchas más funciones y tipos de datos disponibles en los módulos `std::time` y `chrono` para trabajar con fechas y horas en Rust. Ahora veremos una explicación más profunda de cómo funcionan internamente estas funciones.

## Profundizando en la obtención de la fecha actual en Rust

Internamente, `std::time::Instant` es representado por un número de nanosegundos desde la época de Unix. La época de Unix es el 1 de enero de 1970 a las 00:00:00 UTC. Esto significa que `now()` devolverá un número positivo si la hora actual es después de la época de Unix y un número negativo si la hora actual es antes.

Por otro lado, `chrono::DateTime` utiliza una estructura de datos más compleja que incluye información como el año, mes, día, hora, etc. También tiene una función `now()` que devuelve la fecha y hora actual en la zona horaria especificada.

Es importante señalar que, al igual que con cualquier otra tarea de programación, hay múltiples formas de obtener la fecha actual en Rust. Puedes buscar en la documentación oficial o en foros y blogs en línea para encontrar otras alternativas y elegir la que mejor se adapte a tus necesidades.

## Ver también

- Documentación oficial de Rust sobre `std::time`: https://doc.rust-lang.org/std/time/
- Documentación oficial de Rust sobre `chrono`: https://docs.rs/chrono/0.4.19/chrono/

Esperamos que este artículo te haya ayudado a comprender cómo obtener la fecha actual en Rust y por qué es importante para los programadores. ¡No dudes en experimentar con diferentes métodos y bibliotecas para encontrar el que mejor se adapte a tu aplicación!