---
title:                "Analizando una fecha a partir de una cadena de texto"
html_title:           "Bash: Analizando una fecha a partir de una cadena de texto"
simple_title:         "Analizando una fecha a partir de una cadena de texto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

El análisis de una fecha desde una cadena es una operación común en programación para convertir datos de texto en objetos de fecha. Los programadores lo hacen principalmente para manipular, comparar, y almacenar fechas de manera eficiente.

## Cómo Hacerlo:

En Kotlin, puedes usar la clase `DateTimeFormatter` y el método `parse` para parsear una cadena de texto en una fecha. Aquí tienes un ejemplo:

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val formato = DateTimeFormatter.ofPattern("dd-MM-yyyy")
    val fecha = LocalDate.parse("18-06-2022", formato)
    println(fecha)
}
```
La salida será: `2022-06-18`.

## Un Vistazo Mas Profundo:

1. **Contexto Histórico:** En el pasado, en Java (lenguaje en el que se basa Kotlin), la clase más comúnmente utilizada era `SimpleDateFormat`. Pero con el lanzamiento de Java 8, se agregaron nuevas clases de fecha y hora, incluyendo `LocalDate` y `DateTimeFormatter`, que son más fáciles de usar, más flexibles y thread-safe.

2. **Alternativas:** También puedes usar la biblioteca Joda-Time, que proporciona clases adicionales para manejar fechas y horas, pero con Kotlin y Java 8 en adelante, la mayoría de veces será suficiente con `LocalDate`.

3. **Implementación:** El análisis de una fecha se realiza analizando primero cada segmento de la cadena de entrada (día, mes, año, etc.) y luego utilizando estos valores para construir un objeto de fecha. Los patrones de formato le dicen al analizador qué segmentos buscar y en qué orden.

## Ver También:

2. Joda-Time: [Documentación](http://joda-time.sourceforge.net/)
3. Tutorial paso a paso: [Parsear Fechas en Kotlin](https://www.baeldung.com/kotlin-dates)