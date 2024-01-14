---
title:    "Kotlin: Obteniendo la fecha actual"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por qué
La obtención de la fecha actual es una tarea común en la programación. Puede ser necesaria para registrar la hora de una acción, mostrar la fecha en una interfaz de usuario o para realizar cálculos basados en el tiempo. Afortunadamente, con Kotlin es muy sencillo obtener la fecha actual.

## Cómo hacerlo
Para obtener la fecha actual en Kotlin, podemos utilizar la clase `java.util.Date` junto con la función `Date()` que nos devuelve un objeto `Date` con la fecha y hora actuales. Luego, podemos imprimir la fecha utilizando el método `toString()`.

```Kotlin
val currentDateTime = Date()
println(currentDateTime.toString())
```

La salida de este código sería algo como: `Tue Jun 22 17:59:05 UTC 2021`.

Si deseamos personalizar el formato de la fecha, podemos utilizar la clase `java.text.SimpleDateFormat`. Por ejemplo, si queremos imprimir la fecha en formato "día/mes/año", podemos hacer lo siguiente:

```Kotlin
val dateFormat = SimpleDateFormat("dd/MM/yyyy")
val currentDateTime = Date()
println(dateFormat.format(currentDateTime))
```

La salida sería: `22/06/2021`.

## Profundizando
Si bien la clase `Date` es una forma sencilla de obtener la fecha actual, también podemos utilizar la clase `Calendar` para realizar operaciones más avanzadas con fechas y horas. Por ejemplo, podemos obtener el día de la semana actual en formato numérico utilizando el método `get()` y la constante `Calendar.DAY_OF_WEEK`.

```Kotlin
val calendar = Calendar.getInstance()
val currentDayOfWeek = calendar.get(Calendar.DAY_OF_WEEK)
println(currentDayOfWeek)
```

La salida sería un número del 1 al 7, donde 1 representa el domingo y 7 el sábado.

## Véase también
- [Documentación de la clase Date](https://developer.android.com/reference/java/util/Date)
- [Documentación de la clase SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [Documentación de la clase Calendar](https://developer.android.com/reference/java/util/Calendar)