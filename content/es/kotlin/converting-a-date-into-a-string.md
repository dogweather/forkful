---
title:    "Kotlin: Convirtiendo una fecha en una cadena"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una fecha en una cadena de texto es una práctica común en la programación, especialmente al mostrar fechas en un formato legible para el usuario final. También puede ser útil para almacenar fechas en bases de datos o comunicarse con servicios web. En este artículo, aprenderemos cómo convertir una fecha en una cadena de texto utilizando Kotlin.

## Cómo hacerlo

Para convertir una fecha en una cadena de texto en Kotlin, utilizaremos la función `format` de la clase `DateTimeFormatter` de la librería de Java. Primero, debemos importar la librería con la siguiente línea de código:

```Kotlin
import java.time.format.DateTimeFormatter
```

Luego, creamos un objeto `DateTimeFormatter` con el formato deseado. Por ejemplo, si queremos mostrar la fecha en el formato 'dd/MM/yyyy', utilizaremos el siguiente código:

```Kotlin
val formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
```

Después, podemos utilizar la función `format` para convertir una fecha en una cadena de texto utilizando nuestro objeto `DateTimeFormatter`. Por ejemplo, si queremos convertir la fecha actual, podemos utilizar el objeto `LocalDate` de la librería `java.time` y la función `now()`:

```Kotlin
val fecha = LocalDate.now()
val fechaCadena = formatter.format(fecha)
```

La variable `fechaCadena` ahora contiene la fecha actual en el formato especificado. Si queremos imprimir esta cadena en la consola, podemos utilizar la función `println`:

```Kotlin
println(fechaCadena)
```

El resultado de este código sería algo como '21/08/2021'.

## Profundizando

Además del formato utilizado en el objeto `DateTimeFormatter`, también podemos personalizar otras propiedades de nuestra cadena de texto, como añadir el día de la semana o la hora. Por ejemplo, si queremos mostrar el día de la semana junto con la fecha, podemos añadir 'EEEE, dd/MM/yyyy' en nuestro arreglo de formato. La letra 'E' representa el día de la semana en una abreviación de tres letras. Podemos utilizar más de una 'E' para mostrar el día de la semana completo ('EEEE') o solo la primera letra ('E').

Otra propiedad interesante que podemos utilizar es la 'zona horaria'. Si queremos mostrar la fecha en una zona horaria específica, podemos utilizar la función `withZone` en nuestro objeto `DateTimeFormatter` y especificar la zona horaria deseada. Por ejemplo, si queremos mostrar la fecha en 'New York', podemos utilizar la siguiente línea de código:

```Kotlin
val formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy").withZone(ZoneId.of("America/New_York"))
```

## Ver también

- [Documentación oficial de Kotlin sobre el objeto DateTimeFormatter] (https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-date-time/-date-time-formatter/)
- [Tutorial de Javatpoint sobre cómo convertir una fecha en una cadena de texto en Java] (https://www.javatpoint.com/java-date-to-string)
- [Stack Overflow question sobre cómo mostrar diferentes formatos de fechas en Kotlin] (https://stackoverflow.com/questions/25703336/kotlin-formatting-java-date)