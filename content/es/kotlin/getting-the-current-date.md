---
title:                "Obteniendo la fecha actual"
html_title:           "Kotlin: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Obtener la fecha actual es una tarea común en la programación. Los programadores suelen hacerlo para tener un registro de cuándo se ejecutó un programa o para mostrar la fecha actual en una aplicación.

## Cómo hacerlo:
Para obtener la fecha actual en Kotlin, puedes usar la función ```Kotlin LocalDate.now()```. Esto devolverá la fecha actual en formato ISO-8601 (ejemplo: 2021-08-23).

Si necesitas la fecha y la hora actual, puedes usar la función ```Kotlin LocalDateTime.now()```. Esta función devuelve un objeto que contiene tanto la fecha como la hora actual en formato ISO-8601 (ejemplo: 2021-08-23T12:00:00).

## Profundizando:
Obtener la fecha actual es una tarea que ha evolucionado a lo largo de los años. Antes de la aparición de los sistemas operativos modernos, los programadores tenían que escribir su propio código para obtener la fecha y hora actual utilizando funciones específicas de lenguajes de programación.

En la actualidad, la mayoría de los lenguajes de programación tienen funciones incorporadas para obtener la fecha y hora actual. Alternativamente, también se pueden utilizar bibliotecas externas para realizar esta tarea.

En Kotlin, la función ```now()``` se basa en el reloj del sistema operativo para obtener la fecha y hora actual. Es importante tener en cuenta que esto significa que la fecha y hora pueden variar dependiendo de la zona horaria en la que se encuentre el dispositivo en el que se ejecuta el programa.

## Véase también:
- [Documentación de Kotlin sobre la clase LocalDate] (https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date/index.html)
- [Documentación sobre la clase LocalDateTime] (https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date-time/index.html)
- [Artículo de blog sobre cómo obtener la fecha actual en Kotlin] (https://blog.mindorks.com/getting-current-date-and-time-in-java-and-kotlin-chronodatetime)