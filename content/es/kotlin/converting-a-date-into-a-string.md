---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "C++: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Conversión de fechas en cadenas significa transformar un objeto de fecha en un formato de texto legible, como por ejemplo "2022-02-28". Los programadores lo hacen para simplificar la visualización y almacenamiento de fechas, y para facilitar su manipulación en diferentes formas en la interfaz de usuario.

## Cómo hacerlo:
Aquí te muestro cómo puedes hacerlo con la biblioteca `DateTimeFormatter` de kotlin. 

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val actualDate = LocalDate.now()

    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val formattedDate = actualDate.format(formatter)

    println(formattedDate)
}
```

La salida será algo como esto:

```2022-02-28```

## Análisis Profundo
En kotlin, el uso `DateTimeFormatter` para formato de fechas es estándar desde la versión de Java 8. Anteriormente, en las versiones de Java 6 y 7, teníamos que usar `SimpleDateFormat` para hacer la misma tarea, pero era más verboso y menos eficiente.

Alternativamente, también puedes usar bibliotecas de terceros como `Joda-Time` y `ThreeTenABP` para esta tarea. Pero dado que `DateTimeFormatter` está incorporado, es la opción preferida en la mayoría de los casos.

La implementación de `DateTimeFormatter` es bastante directa, solo necesitas proporcionar un patrón como "yyyy-MM-dd" al método `ofPattern` y después puedes aplicar este formato a cualquier objeto `LocalDate` usando el método `format`.

## Ver También
Aquí te dejo algunas fuentes útiles para entender mejor el trabajo con fechas en kotlin:

- Tutorial sobre cómo utilizar `DateTimeFormatter`: [link](https://www.journaldev.com/17899/java-simpledateformat-java-date-format)
- Comparación de `SimpleDateFormat` y `DateTimeFormatter`: [link](https://www.baeldung.com/java-8-date-time-intro)