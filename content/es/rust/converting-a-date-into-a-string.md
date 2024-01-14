---
title:    "Rust: Convertir una fecha a una cadena de texto."
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una fecha en una cadena?

Convertir una fecha en una cadena es una tarea común en la programación, ya que a menudo se necesita mostrar una fecha en un formato legible para el usuario. En Rust, existen varias formas de realizar esta conversión, dependiendo de las necesidades específicas del proyecto.

## Cómo hacerlo

Para convertir una fecha en una cadena en Rust, primero necesitamos importar el módulo "chrono", que proporciona funcionalidades para trabajar con fechas y horas. Luego, podemos utilizar el método "format" para convertir la fecha en una cadena con el formato deseado. Por ejemplo:

```Rust
use chrono::prelude::*;

fn main() {
    let fecha = Utc::now();
    let fecha_string = fecha.format("%d/%m/%Y").to_string();
    println!("{}", fecha_string);
}
```

En este ejemplo, estamos utilizando el formato "%d/%m/%Y" que devuelve la fecha en formato día/mes/año. La salida sería algo como "22/10/2021". Podemos cambiar el formato según nuestras necesidades, utilizando distintas combinaciones de símbolos que se encuentran en la documentación de "chrono".

## Profundizando

En el ejemplo anterior, utilizamos el módulo "Utc" para obtener la fecha y hora actuales en formato UTC (Tiempo Universal Coordinado). Sin embargo, también podemos trabajar con otras zonas horarias utilizando los módulos "Local" o "DateTime". Además, el módulo "chrono" proporciona muchas otras funcionalidades, como comparar fechas, sumar o restar días, entre otras.

También es importante tener en cuenta que la librería "chrono" tiene un alto rendimiento y es segura para utilizar en hilos de ejecución simultánea. Esto significa que podemos usarla sin problemas en proyectos más complejos.

## Ver también

- [Documentación sobre el módulo "chrono" en Rust](https://docs.rs/chrono)
- [Tutorial sobre cómo trabajar con fechas y horas en Rust](https://dev.to/deciduously/working-with-dates-and-times-in-rust-1bjp)
- [Más información sobre la gestión de fechas y horas en Rust](https://learning-rust.github.io/docs/a14.date_and_time.html)