---
title:                "Kotlin: Obteniendo la fecha actual"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué
Obtener la fecha actual es una tarea común en la programación. Con Kotlin, podemos hacerlo de manera eficiente y sencilla.

## Cómo hacerlo
Para obtener la fecha actual en Kotlin, podemos utilizar la clase `LocalDate` de la biblioteca estándar de Java. Primero, debemos importarla en nuestro código:
```
import java.time.LocalDate
```

Luego, podemos llamar al método `now()` para obtener la instancia de la fecha actual:
```
val fechaActual = LocalDate.now()
```

Podemos imprimir esta fecha en la consola de la siguiente manera:
```
println(fechaActual)
```

El resultado será algo como esto:
```
2020-11-09
```

También podemos obtener la fecha en un formato específico utilizando el método `format()`. Por ejemplo, si queremos el formato día/mes/año, podemos hacer lo siguiente:
```
val fechaFormateada = fechaActual.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))
println(fechaFormateada)
```

La salida será:
```
09/11/2020
```

## Deep Dive
Si queremos obtener más información de la fecha actual, podemos utilizar los métodos de la clase `LocalDate`. Algunos ejemplos son `getDayOfWeek()`, `getMonth()` y `getYear()`, que nos devolverán el día de la semana, el mes y el año, respectivamente.

Además, podemos realizar operaciones con fechas, como agregar o restar días, semanas, meses o años. Por ejemplo, si queremos obtener la fecha dentro de una semana, podemos hacer lo siguiente:
```
val fechaProximaSemana = fechaActual.plusWeeks(1)
println(fechaProximaSemana)
```

La salida será:
```
2020-11-16
```

También es importante tener en cuenta que la clase `LocalDate` representa una fecha en formato ISO-8601, es decir, en el formato AAAA-MM-DD. Esto facilita la comparación de fechas y hace que sea más fácil trabajar con ellas.

## Ver también
- [Documentación de la clase LocalDate en la biblioteca estándar de Java](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Artículo sobre cómo trabajar con fechas en Kotlin](https://www.baeldung.com/kotlin/dates)