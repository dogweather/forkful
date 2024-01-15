---
title:                "Comparando dos fechas"
html_title:           "Kotlin: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué
Si estás trabajando en un proyecto que involucra fechas, puede ser necesario comparar dos fechas. Esto te permite determinar si una fecha es anterior, posterior o igual a otra, lo cual es útil en muchas aplicaciones, como desde la planificación de citas hasta la compra de billetes de avión.

## Cómo hacerlo
Para comparar dos fechas en Kotlin, puedes utilizar el método .compareTo (), que devuelve un valor entero que indica si la fecha es anterior, posterior o igual a la otra. Veamos un ejemplo de cómo usar esto:

```Kotlin
val primeraFecha = LocalDate.of(2021, 3, 15)
val segundaFecha = LocalDate.of(2021, 3, 20)

val resultado = primeraFecha.compareTo(segundaFecha)

println(resultado)
```
Esto imprimirá un valor de -1, lo que significa que la primera fecha es anterior a la segunda fecha. También puedes utilizar el método .isEqual () para determinar si dos fechas son iguales. Echemos un vistazo a un ejemplo:

```Kotlin
val primeraFecha = LocalDate.of(2021, 3, 15)
val segundaFecha = LocalDate.of(2021, 3, 15)

val resultado = primeraFecha.isEqual(segundaFecha)

println(resultado)
```
En este caso, el resultado será true, ya que ambas fechas son iguales.

## Profundizando
Si deseas comparar fechas con una precisión más granular, como comparar horas o minutos también, puedes utilizar la clase LocalDateTime. Esta clase combina una fecha y una hora en un solo objeto, lo que te permite comparar fechas y horas juntas. Aquí hay un ejemplo de cómo comparar dos fechas con horas y minutos utilizando objetos LocalDateTime:

```Kotlin
val primeraFecha = LocalDateTime.of(2021, 3, 15, 10, 30)
val segundaFecha = LocalDateTime.of(2021, 3, 15, 11, 15)

val resultado = primeraFecha.compareTo(segundaFecha)

println(resultado)
```
En este caso, el resultado será -1, ya que la primera fecha es anterior a la segunda en términos de horas y minutos también.

## Ver también
- [Documentación oficial de comparación de fechas en Kotlin](https://kotlinlang.org/docs/comparison-operators.html#equals-and-compares)
- [Tutorial de Kotlin sobre manipulación de fechas](https://www.baeldung.com/kotlin-dates)
- [Uso de las clases Date e Instant en Kotlin](https://medium.com/@szaboa/%C3%ADndice-de-java-en-kotlin-uso-de-las-clases-date-e-instant-a5922c9ec440)