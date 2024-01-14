---
title:    "Kotlin: Obteniendo la fecha actual"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué obtener la fecha actual es importante

Obtener la fecha actual es una tarea común en la programación. Puede ser útil en muchas situaciones, como registrar la fecha en que se realizó una acción o mostrar la fecha de hoy en un calendario. También puede ser necesario para realizar cálculos basados en la fecha actual, como hacer una reserva para una fecha específica.

## Cómo obtener la fecha actual en Kotlin

En Kotlin, obtener la fecha actual se puede hacer utilizando la clase `Date` y el método `now()`. Aquí hay un ejemplo de cómo se vería esto en código:

```Kotlin
val fechaActual = Date().now()
println("La fecha de hoy es: $fechaActual")
```
El resultado sería algo como: `La fecha de hoy es: Wed Jul 21 20:43:21 GMT 2021`.

También puedes especificar el formato de fecha que deseas utilizando la clase `SimpleDateFormat`. Aquí hay un ejemplo de cómo se vería esto en código:

```Kotlin
val fechaActual = SimpleDateFormat("dd-MM-yyyy").format(Date().now())
println("La fecha de hoy es: $fechaActual")
```
El resultado sería algo como: `La fecha de hoy es: 21-07-2021`.

## Análisis detallado sobre cómo obtener la fecha actual

Ahora, profundicemos un poco más en cómo funciona el código para obtener la fecha actual en Kotlin. La clase `Date` representa una instantánea de tiempo específica, y el método `now()` devuelve una instancia de `Date` basada en la hora actual del sistema. Luego, al especificar un formato de fecha con la clase `SimpleDateFormat`, podemos formatear la fecha a nuestra conveniencia.

Es importante tener en cuenta que la clase `Date` está en desuso y se recomienda utilizar la clase `LocalDate` de la nueva API de fechas en Kotlin. También hay otras opciones para obtener la fecha actual, como utilizando la función `now()` de la clase `Clock` o el método `getCurrentDateTime()` de la biblioteca `java.time`.

## Ver También

- [Cómo utilizar la nueva API de fechas en Kotlin](https://www.baeldung.com/kotlin/java-time-api)
- [Documentación oficial de la clase `Date` en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/)
- [Tutorial sobre manipulación de fechas en Kotlin](https://www.programmerall.com/article/1616638619/)