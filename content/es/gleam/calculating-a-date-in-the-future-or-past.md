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

## ¡Da un salto hacia el futuro o el pasado: Cómo calcular fechas con Gleam!

Si alguna vez te has preguntado cuántos días faltan para el próximo cumpleaños de tu mejor amigo o cuántos días han pasado desde que nació tu mascota, entonces este artículo es para ti. Gleam, un lenguaje de programación funcional creado por el equipo de investigación de Galois, te permite realizar cálculos precisos de fechas en el pasado y en el futuro. ¡Así que sigue leyendo y descubre cómo dar un salto en el tiempo con Gleam!

## ¿Por qué?

¿Por qué te gustaría calcular fechas en el futuro o en el pasado? Bueno, hay muchas razones. Tal vez necesitas saber cuándo se cumple un plazo importante o quieres saber cuántos días te quedan en tu período de vacaciones. O tal vez eres un aficionado a la genealogía y quieres calcular cuántas generaciones han pasado desde que nació tu bisabuelo. Sea cual sea la razón, Gleam te ofrece una forma fácil y precisa de calcular fechas.

## Cómo hacerlo

Primero, importa el módulo `Calendar.Date` en tu programa:

```Gleam
import Calendar.Date
```

A continuación, puedes utilizar la función `add_days` para calcular una fecha en el futuro o en el pasado:

```Gleam
let date = Calendar.Date.add_days(
  today(),
  30
)
```

En este ejemplo, hemos usado la función `today` para obtener la fecha actual como punto de partida y luego hemos añadido 30 días a esta fecha usando `add_days`. Puedes cambiar el número de días según tus necesidades. Además, puedes restar días en lugar de añadirlos para obtener una fecha en el pasado.

Si quieres obtener una fecha específica en el pasado o en el futuro, puedes utilizar la función `make` y especificar el año, mes y día como argumentos. Por ejemplo:

```Gleam
let date = Calendar.Date.make(1995, 8, 24)
```

También puedes hacer cálculos más complejos como sumar años o meses a una fecha determinada utilizando las funciones `add_years` y `add_months`.

## Profundizando en el cálculo de fechas

Gleam utiliza el calendario gregoriano para calcular fechas, lo que significa que no se tendrán en cuenta los cambios en el calendario que se han hecho a lo largo de la historia (como el cambio del calendario juliano al gregoriano en 1582). Además, Gleam no puede manejar fechas anteriores al 1 de enero de 1582.

Si te interesa la matemática detrás de los cálculos de fechas, puedes echar un vistazo a cómo Gleam utiliza algoritmos y funciones para realizar estas operaciones. ¡Quién sabe, podría ser una buena oportunidad para mejorar tus habilidades matemáticas y de programación al mismo tiempo!

## Consulta también

- [Documentación de Gleam sobre fechas](https://gleam.run/std/calendar.date.html)
- [Tutorial de Gleam](https://gleam.run/book/tour/introduction.html)
- [Comunidad de Gleam en Discord](https://discord.gg/7Rptjwu)