---
title:    "Gleam: Comparación de dos fechas"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Por qué comparar dos fechas en Gleam?

Comparar dos fechas es una tarea común en la programación, especialmente al trabajar con datos que tienen una componente temporal. Con Gleam, puedes realizar fácilmente esta comparación y obtener resultados precisos.

## Cómo hacerlo

La comparación de dos fechas en Gleam se realiza utilizando el operador de comparación "==" entre dos valores de tipo Date. Veamos un ejemplo:

```Gleam
let primeraFecha = Date.create(2021, 12, 1)
let segundaFecha = Date.create(2021, 11, 15)

if primeraFecha == segundaFecha {
  IO.print("Las dos fechas son iguales")
} else {
  IO.print("Las fechas son diferentes")
}
```

El resultado de este código sería "Las fechas son diferentes", ya que las dos fechas no son la misma. También se pueden utilizar otros operadores de comparación como "<" o ">=" para realizar comparaciones basadas en la fecha y la hora.

## Profundizando

Gleam proporciona una variedad de funciones y métodos para trabajar con fechas, lo que te permite realizar operaciones más complejas. Por ejemplo, la función `Date.diff_in_days` se puede utilizar para calcular la diferencia en días entre dos fechas. Además, Gleam tiene un módulo llamado `Calendar` que proporciona funciones para trabajar con fechas y tiempos de manera más detallada. ¡Explora estas opciones y descubre cómo puedes utilizarlas en tus proyectos!

## Ver también

- [Documentación oficial de fecha y hora en Gleam](https://gleam.run/modules/date.html)
- [Ejemplos de código para comparar fechas en Gleam](https://github.com/gleam-lang/gleam/blob/master/examples/dates.gleam)
- [Tutorial sobre cómo trabajar con fechas en Gleam](https://gleam.run/tutorials/dates.html)