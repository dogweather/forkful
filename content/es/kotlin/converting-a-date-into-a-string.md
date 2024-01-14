---
title:                "Kotlin: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una fecha a una cadena de texto puede ser útil en situaciones en las que se necesite mostrar la fecha de una manera más legible para el usuario. Por ejemplo, en una aplicación de reserva de citas, se podría mostrar la fecha de la cita en formato de texto para que sea más fácil de entender.

## Cómo hacerlo

Primero, importemos el paquete de manejo de fechas en Kotlin:

```Kotlin
import java.time.format.DateTimeFormatter
```

Luego, creamos una instancia del objeto `LocalDateTime` con la fecha que queremos convertir:

```Kotlin
val date = LocalDateTime.of(2021, 4, 15, 10, 30, 0)
```

Ahora, usamos el método `format()` y pasamos como argumento el formato de fecha deseado:

```Kotlin
val formattedDate = date.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))
println(formattedDate) // output: 15/04/2021
```

Podemos usar diferentes patrones en el método `DateTimeFormatter` según el formato de fecha que necesitemos. Por ejemplo, si queremos mostrar la fecha con el nombre del mes en lugar de su número, podemos usar el patrón "dd MMMM yyyy" y obtendremos "15 abril 2021".

## Profundizando

La clase `DateTimeFormatter` nos permite crear patrones personalizados para la conversión de fechas. Algunos de los símbolos más comunes que se usan en patrones son:

- `y`: año
- `M`: mes
- `d`: día
- `H`: hora (formato de 24 horas)
- `h`: hora (formato de 12 horas)
- `m`: minuto
- `s`: segundo

Podemos combinar estos símbolos con otros caracteres para obtener el formato deseado. Por ejemplo, si queremos mostrar las horas y minutos con un cero al inicio si son menores a 10, podemos usar el patrón "HH:mm" y obtendremos "10:30" en lugar de "10: 30".

## Ver también

- [Documentación oficial de Kotlin sobre `DateTimeFormatter`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-date-time-formatter/)
- [Tutoriales de manejo de fechas en Kotlin](https://www.kotlinresources.com/library/kotlin-datetime/)