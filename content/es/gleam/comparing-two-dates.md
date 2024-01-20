---
title:                "Comparando dos fechas"
html_title:           "C#: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Comparar dos fechas es analizar cuál es anterior o posterior a la otra. Los programadores lo hacen para ejecutar ciertas acciones basadas en estos resultados, especialmente en la gestión de datos.

## Cómo hacerlo:
Aquí hay un ejemplo simple de cómo se puede hacer esto en Gleam:

```gleam
fn main() {
  let fecha1 = Date.from(2020, 4, 7)
  let fecha2 = Date.from(2022, 8, 14)

  case fecha1.compare(fecha2) {
    Equal   -> io.print("Las fechas son iguales")
    Greater -> io.print("La fecha1 es mayor que la fecha2")
    Less    -> io.print("La fecha1 es menor que la fecha2")
  }
}
```

El resultado de la ejecución del código anterior será `"La fecha1 es menor que la fecha2"` porque la primera fecha ocurre antes que la segunda.

## Inmersión Profunda
Historia: Comparar dos fechas es un problema común en programación desde los primerizos tiempos de la informática. Gleam gestiona esta tarea de manera eficiente, apoyándose en su tipo de datos integrado `Date` y su función `compare`.

Alternativas: En otros lenguajes de programación, como JavaScript o Python, la comparación de fechas puede realizarse con operadores de comparación estándar, aunque puede resultar más complicado debido a las peculiaridades de zona horaria y formato de fechas.

Detalles de Implementación: La función `compare` implementada en Gleam devuelve uno de los tres valores predefinidos `Equal`, `Greater`o `Less` lo que facilita mucho la comprensión y el uso de la comparación de fechas.

## Ver También
2. [Gleam en GitHub](https://github.com/gleam-lang/gleam)
3. [Comparar fechas en otros lenguajes](https://stackoverflow.com/questions/tagged/date-comparison)
   
Para una guía más profunda sobre la programación en Gleam, puedes consultar el libro [Gleam programming for beginner](https://gleam.run/book).