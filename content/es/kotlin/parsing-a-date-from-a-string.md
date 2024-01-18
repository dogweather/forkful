---
title:                "Analizando una fecha desde una cadena."
html_title:           "Kotlin: Analizando una fecha desde una cadena."
simple_title:         "Analizando una fecha desde una cadena."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué es importante?

Parsear una fecha a partir de una cadena de texto es el proceso de convertir una fecha que se encuentra en formato de texto a un tipo de dato de fecha legible para la computadora. Los programadores realizan esta tarea para poder manipular y trabajar con fechas en sus programas de manera más eficiente.

## Cómo:

```Kotlin
val fecha = "12/08/2021" //fecha en formato de texto
val fechaParseada = LocalDate.parse(fecha, DateTimeFormatter.ofPattern("dd/MM/yyyy")) //parsear la fecha a formato de fecha LocalDate
println(fechaParseada) //salida: 2021-08-12
```

Una vez que tenemos la fecha parseada, podemos utilizarla para realizar operaciones como calcular el tiempo transcurrido entre dos fechas, comparar fechas, o mostrar la fecha en un formato diferente.

## Detalles a profundidad:

Parsear fechas a partir de cadenas de texto ha sido una tarea común en la programación desde hace mucho tiempo. Anteriormente, se utilizaba principalmente el lenguaje de programación 'C' para realizar esta tarea, pero con el avance de la tecnología, se han desarrollado herramientas más eficientes y específicas para parsear fechas, como librerías y clases en lenguajes de programación modernos como Java y Kotlin.

En Kotlin, podemos utilizar la clase LocalDate para almacenar una fecha en formato de objeto. Luego, utilizamos la clase DateTimeFormatter para especificar el formato en el que se encuentra la fecha en la cadena de texto y realizar el proceso de parsing.

Otra alternativa para parsear fechas es utilizar librerías de terceros como Joda-Time o la librería de Android, que ofrece diferentes métodos para manipular y trabajar con fechas en la programación.

## Ver también:

- [Documentación de la clase LocalDate en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/)
- [Métodos de la clase DateTimeFormatter en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-date-time-formatter/)
- [Información sobre Joda-Time](https://www.joda.org/joda-time/)
- [Librería de Android para manipular fechas](https://developer.android.com/reference/java/util/Date)