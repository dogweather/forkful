---
title:                "Rust: Obteniendo la fecha actual"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

Rust es un lenguaje de programación moderno y eficiente, que se ha vuelto cada vez más popular entre los desarrolladores en los últimos años. Una de las razones detrás de su creciente popularidad es su capacidad de manejar concurrencia y paralelismo de forma segura y eficiente. Hasta ahora, hemos aprendido sobre cómo Rust puede manejar concurrencia, pero ahora ampliemos nuestros conocimientos con algo práctico: ¿cómo podemos obtener la fecha actual en Rust?

## Cómo hacerlo

Usando la biblioteca estándar de Rust `chrono`, podemos obtener la fecha y hora actual usando el siguiente código:

```Rust 
use chrono::Local;

let fecha = Local::now(); 
println!("La fecha y hora actual es: {}", fecha); 
```
La salida de este código sería algo como esto:

```
La fecha y hora actual es: 2021-09-15 14:30:00.023201 UTC
```

Pero, ¿qué sucede si queremos un formato de fecha diferente? Hay muchas formas de dar formato a una fecha en Rust, pero aquí hay un ejemplo utilizando `strftime` para obtener una fecha en formato "día/mes/año":

```Rust
use chrono::{Local, Datelike};

let date = Local::now();
let formatted_date = date.strftime("%d/%m/%Y").unwrap_or("Fecha no disponible".to_string());
println!("La fecha actual es: {}", formatted_date);
```

La salida de este código sería la fecha actual en formato "día/mes/año", por ejemplo: 15/09/2021.

## Profundizando en el tema

Ahora que sabemos cómo obtener la fecha actual en Rust, es importante saber que `chrono` también permite trabajar con diferentes zonas horarias y fechas históricas. Además, también tiene funciones útiles para trabajar con duraciones de tiempo, como calcular la diferencia entre dos fechas. Si quieres aprender más sobre `chrono`, puedes consultar su documentación [aquí](https://docs.rs/chrono/latest/chrono/).

## Ver también

- [Documentación de la biblioteca chrono](https://docs.rs/chrono/latest/chrono/)
- [Código de ejemplo para obtener la fecha actual en Rust](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=f1534bb83dd8ed6f67e2b76b9c23059b)
- [Tutorial para trabajar con fechas y horas en Rust](https://blog.logrocket.com/handling-dates-and-times-in-rust/)