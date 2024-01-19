---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Gleam: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
Calcular una fecha en el futuro o el pasado es una tarea común en programación. Se utiliza para todo, desde la programación de recordatorios hasta el seguimiento de fechas de vencimiento.

## Cómo se hace:
Aquí te muestro un ejemplo de cómo se puede calcular una fecha futura/pasada con Gleam:

```Gleam
import gleam/calendar.{Date, Duration}

fn main() {
  let today = Date.today()
  // Para sumar días futuros
  let future_date = Date.add_days(today, 5)
  // Para restar días pasados
  let past_date = Date.sub_days(today, 5)
  Io.println(future_date)
  Io.println(past_date)
}
```
El código anterior imprime la fecha de cinco días en el futuro y cinco días en el pasado a partir del día actual.

## Análisis profundo
Históricamente, el cálculo de fechas se ha utilizado en diversas aplicaciones para rastrear eventos, recordar fechas importantes y calcular duraciones. En términos de alternativas, muchos lenguajes de programación, incluido Gleam, proporcionan bibliotecas de fechas y horas incorporadas para este propósito.
En cuanto a los detalles de la implementación, Gleam utiliza la función `add_days` para calcular una fecha en el futuro y la función `sub_days` para calcular una fecha en el pasado.

## Ver también
* Documentación oficial de Gleam: https://gleam.run/docs/
* Otra ejemplos de código Gleam: https://gleam.run/examples/
* The Book of Gleam: https://book.gleam.run/ 
Por supuesto, la mejor manera de aprender es practicando. Así que atrévete a explorar y ensayar con diferentes funciones de Gleam para manejar y manipular fechas. ¡Buena suerte!