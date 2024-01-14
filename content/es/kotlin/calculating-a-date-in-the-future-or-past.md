---
title:    "Kotlin: Calculando una fecha en el futuro o pasado"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Por qué programar en Kotlin?

Programar en Kotlin puede ser una excelente opción para aquellos que buscan un lenguaje de programación moderno y fácil de aprender. Además, la sintaxis simple y concisa de Kotlin hace que sea más eficiente y rápido de escribir código. Una de las tareas habituales en la programación es el cálculo de fechas en el futuro o en el pasado. En este artículo, te mostraremos cómo hacerlo con Kotlin.

## Cómo calcular una fecha en el futuro o pasado en Kotlin

Para calcular una fecha en el futuro o en el pasado en Kotlin, primero debes crear un objeto de tipo `Calendar` y establecer la fecha actual usando el método `getInstance()`. Luego, puedes usar el método `add()` para sumar o restar días, meses o años a la fecha actual. Por ejemplo:

```
Kotlin val currentDate = Calendar.getInstance() currentDate.add(Calendar.DAY_OF_MONTH, 7) // suma 7 días a la fecha actual
```

También puedes establecer directamente una fecha específica usando el método `set()` y luego realizar la operación de suma o resta. A continuación, se muestra un ejemplo de cómo establecer una fecha en el futuro:

```
val futureDate = Calendar.getInstance() futureDate.set(Calendar.YEAR, 2022) // establece el año en 2022 futureDate.add(Calendar.MONTH, 3) // suma 3 meses a la fecha establecida
```

Puedes imprimir la fecha resultante utilizando el método `get()` y especificando el campo de fecha que deseas obtener. Por ejemplo:

```
println("Fecha en el futuro: ${futureDate.get(Calendar.DAY_OF_MONTH)}/${futureDate.get(Calendar.MONTH)}/${futureDate.get(Calendar.YEAR)}")
// salida: Fecha en el futuro: 20/1/2022
```

## Profundizando en el cálculo de fechas en Kotlin

Kotlin también ofrece la clase `LocalDate` para manejar fechas, lo que simplifica aún más el cálculo de fechas en el futuro o pasado. Esta clase proporciona métodos como `plus()` y `minus()` que facilitan la suma y resta de fechas. Por ejemplo:

```
val currentDate = LocalDate.now() val futureDate = currentDate.plusDays(7) // suma 7 días a la fecha actual println("Fecha en el futuro: $futureDate") // salida: Fecha en el futuro: 2021-05-21
```

También puedes establecer directamente una fecha utilizando el constructor y luego realizar operaciones de suma o resta. A continuación, se muestra un ejemplo de cómo establecer una fecha en el pasado:

```
val pastDate = LocalDate.of(2019, Month.OCTOBER, 15) // establece la fecha en 15 de octubre de 2019 val newDate = pastDate.minusMonths(5) // resta 5 meses a la fecha establecida println("Fecha en el pasado: $newDate") // salida: Fecha en el pasado: 2019-05-15
```

## Ver también

- [Documentación oficial de Kotlin](https://kotlinlang.org/docs/home.html)
- [Tutorial de Kotlin para principiantes](https://www.devexperto.com/kotlin-tutorial-basico/)
- [Cómo trabajar con fechas en Kotlin](https://www.baeldung.com/kotlin-dates)

¡Felicidades! Ahora sabes cómo calcular fechas en el futuro o en el pasado utilizando Kotlin. Esperamos que este artículo te haya sido útil y puedas aplicar este conocimiento en tus futuros proyectos. ¡Buena suerte en tu camino de aprendizaje de Kotlin!