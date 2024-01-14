---
title:                "Kotlin: Convertir una fecha en una cadena"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Por qué
En la programación, es común tener que lidiar con fechas y horas en diferentes formatos. Convertir una fecha a una cadena de caracteres es una tarea frecuente que puede ser necesaria para mostrar información en un formato específico o para guardar en una base de datos. En esta publicación, aprenderemos cómo convertir una fecha en una cadena de caracteres en Kotlin.

# Cómo hacerlo
Para convertir una fecha en una cadena de caracteres en Kotlin, podemos utilizar la función `format` del objeto `SimpleDateFormat` de la clase `java.text`. A continuación, se muestra un ejemplo paso a paso:

```Kotlin
// Importar clase SimpleDateFormat
import java.text.SimpleDateFormat

// Crear un objeto SimpleDateFormat con el formato deseado
val dateFormat = SimpleDateFormat("dd/MM/yyyy")

// Obtener fecha actual
val fechaActual = Date()

// Utilizar la función format para convertir la fecha en una cadena de caracteres
val fechaString = dateFormat.format(fechaActual)

// Imprimir la cadena resultante
println(fechaString)

// Salida: 01/07/2021
```

# Profundizando más
La función `format` acepta diferentes patrones para formatear la fecha en la cadena de caracteres resultante. Por ejemplo, si queremos incluir la hora y los minutos, podemos utilizar el patrón "dd/MM/yyyy HH:mm". También podemos utilizar letras específicas para mostrar el día de la semana o el mes en palabras en lugar de números.

Además, en lugar de utilizar `Date()` para obtener la fecha actual, podemos pasar una fecha específica como parámetro a la función `format`. Esto es útil si queremos convertir una fecha de una variable o de una base de datos en una cadena de caracteres con un formato determinado.

Otra opción para convertir una fecha en una cadena de caracteres es utilizar la función `toString` de la clase `Date`. Sin embargo, esto devuelve la fecha en un formato estandarizado que puede no ser adecuado para nuestras necesidades específicas.

# Ver también
- [Documentación oficial de SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [Cómo trabajar con fechas y horas en Kotlin](https://kotlinlang.org/docs/dates.html)
- [Ejemplos de patrones de formato de fecha y hora](https://www.tutorialspoint.com/java/text_simpledateformat.htm)