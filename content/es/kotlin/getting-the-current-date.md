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

## ¿Por qué obtener la fecha actual en Kotlin?

A veces, necesitamos obtener la fecha actual en nuestra aplicación Kotlin para realizar tareas específicas, como guardar registros de tiempo o establecer plazos de vencimiento. Obtener la fecha actual también puede ser útil para garantizar que nuestro código se ejecute de manera eficiente según la fecha en que se esté ejecutando.

## Cómo obtener la fecha actual en Kotlin

Para obtener la fecha actual en Kotlin, podemos usar la clase `LocalDate` de la biblioteca estándar de Kotlin. Aquí hay un ejemplo de cómo usarlo en nuestro código.

Primero, debemos importar la clase `LocalDate` en nuestro archivo:

```
import java.time.LocalDate
```

Luego, podemos llamar al método `now()` de `LocalDate` para obtener la fecha actual:

```
val currentDate = LocalDate.now()
```

Esto devolverá la fecha actual en formato `YYYY-MM-DD`. También podemos especificar una zona horaria específica si es necesario, como en el siguiente ejemplo:

```
val currentDate = LocalDate.now(ZoneId.of("America/New_York"))
```

Para obtener la fecha actual en un formato diferente, podemos usar el método `format()` y especificar un `DateTimeFormatter` personalizado. Por ejemplo, si queremos obtener la fecha actual en formato `MMM dd, yyyy`, podemos hacer lo siguiente:

```
val currentDate = LocalDate.now()
val formatter = DateTimeFormatter.ofPattern("MMM dd, yyyy")
println(currentDate.format(formatter))
```

El resultado será algo como `Feb 05, 2020`.

Para obtener la fecha actual en una zona horaria específica y en un formato personalizado, podemos combinar los dos enfoques anteriores:

```
val currentDate = LocalDate.now(ZoneId.of("Europe/Berlin"))
val formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
println(currentDate.format(formatter))
```

Esto devolverá la fecha actual en formato `dd/MM/yyyy` en la zona horaria de Berlín.

## Profundizando en la obtención de la fecha actual

Además de la clase `LocalDate`, Kotlin también tiene otras clases útiles para trabajar con fechas y horas, como `LocalTime` y `LocalDateTime`. También hay una serie de métodos y propiedades disponibles en estas clases para convertir y manipular fechas y horas.

Para obtener más información sobre cómo trabajar con fechas y horas en Kotlin, puedes consultar la documentación oficial: <https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/index.html>.

## Ver también

- Documentación oficial de Kotlin sobre trabajo con fechas y horas: <https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/index.html>
- Tutorial en línea sobre cómo trabajar con fechas en Kotlin: <https://www.baeldung.com/kotlin-datetime>
- Ejemplos de código para trabajar con fechas en Kotlin: <https://github.com/Kotlin/kotlin-examples/tree/master/examples/datetime>