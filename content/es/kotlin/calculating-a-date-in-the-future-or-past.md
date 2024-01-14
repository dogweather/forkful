---
title:    "Kotlin: Calculando una fecha en el futuro o pasado"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cómo saber la fecha exacta de algún evento importante en el futuro o pasado? ¡Aprende a calcular fechas en Kotlin!

## Cómo

La función `add()` en la clase `Calendar` de Kotlin nos permite agregar días, meses o años a una fecha determinada. Por ejemplo, si queremos saber la fecha exacta de un evento que ocurre 2 semanas después de hoy, podemos usar el siguiente código:

```Kotlin
val hoy = Calendar.getInstance()
hoy.add(Calendar.WEEK_OF_YEAR, 2)
println("La fecha del evento será: ${hoy.time}")
```

La salida sería algo como: "La fecha del evento será: Thu May 06 15:09:32 GMT 2021".

Si en cambio queremos calcular una fecha en el pasado, podemos usar la función `set()` en lugar de `add()`. Por ejemplo, si queremos saber qué día fue hace 3 años exactamente de hoy, podemos usar el siguiente código:

```Kotlin
val hoy = Calendar.getInstance()
hoy.set(Calendar.YEAR, hoy.get(Calendar.YEAR)-3)
println("La fecha de hace 3 años fue: ${hoy.time}")
```

La salida sería algo como: "La fecha de hace 3 años fue: Sun May 02 15:09:32 GMT 2018".

También podemos especificar los campos de la fecha que queremos modificar en lugar de solo los años. Por ejemplo, si queremos saber la fecha que será dentro de 5 meses y 2 días a partir de hoy, podemos usar el siguiente código:

```Kotlin
val hoy = Calendar.getInstance()
hoy.add(Calendar.MONTH, 5)
hoy.add(Calendar.DAY_OF_YEAR, 2)
println("La fecha será: ${hoy.time}")
```

La salida sería algo como: "La fecha será: Tue Oct 05 15:09:32 GMT 2021".

## Deep Dive

Para realizar cálculos más complejos, podemos usar la clase `LocalDate` que nos brinda Kotlin. Esta clase nos permite realizar cálculos y conversión de fechas sin necesidad de utilizar la clase `Calendar`.

Por ejemplo, si queremos calcular la fecha exacta de un evento que ocurre en 2 años, 3 meses y 15 días desde hoy, podemos usar el siguiente código:

```Kotlin
val hoy = LocalDate.now()
val fechaEvento = hoy.plusYears(2).plusMonths(3).plusDays(15)
println("La fecha del evento será: $fechaEvento")
```

La salida sería algo como: "La fecha del evento será: 2023-08-17".

También podemos obtener el número de días, meses o años entre dos fechas determinadas usando el método `between()`. Por ejemplo, si queremos saber cuántos días han pasado desde nuestro último cumpleaños, podemos usar el siguiente código:

```Kotlin
val ultimoCumpleaños = LocalDate.of(2020, 5, 4)
val hoy = LocalDate.now()
val diasDesdeUltimoCumpleaños = ChronoUnit.DAYS.between(ultimoCumpleaños, hoy)
println("Han pasado $diasDesdeUltimoCumpleaños días desde mi último cumpleaños")
```

La salida sería algo como: "Han pasado 366 días desde mi último cumpleaños" (teniendo en cuenta que el ejemplo fue escrito en un año bisiesto).

## Ver también

- Documentación oficial de Kotlin sobre la clase Calendar: https://developer.android.com/reference/java/util/Calendar
- Documentación oficial de Kotlin sobre la clase LocalDate: https://developer.android.com/reference/java/time/LocalDate