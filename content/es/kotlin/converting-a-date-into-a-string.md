---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "Kotlin: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una fecha a una cadena en Kotlin?

Con la popularidad creciente de Kotlin como lenguaje de programación, es importante conocer sus diferentes funcionalidades y cómo pueden facilitar nuestro trabajo como desarrolladores. La conversión de una fecha a una cadena es una habilidad básica que nos permite mostrar fechas de manera legible para el usuario y es esencial en muchos proyectos.

## Cómo hacerlo en Kotlin

Para convertir una fecha a una cadena en Kotlin, podemos utilizar la función `format()` de la clase `SimpleDateFormat`. Esta función toma dos argumentos, el primer argumento es un patrón que define el formato deseado de la fecha y el segundo es la fecha que se desea convertir.

```Kotlin
val sdf = SimpleDateFormat("EEE, MMM d, yyyy") // Definimos el patrón de la fecha deseada
val date = Date() // Obtenemos la fecha actual
val formattedDate = sdf.format(date) // Convertimos la fecha a una cadena usando el patrón definido
println(formattedDate) // Imprime "Fri, Oct 8, 2021"
```

También podemos usar la función `parse()` de la misma clase para convertir una cadena en formato de fecha en un objeto `Date`.

```Kotlin
val sdf = SimpleDateFormat("dd/MM/yyyy") // Definimos el patrón del formato de fecha de la cadena
val dateString = "08/10/2021" // Creamos una cadena con la fecha deseada
val date = sdf.parse(dateString) // Convertimos la cadena en un objeto Date
println(date) // Imprime "Fri Oct 08 00:00:00 CEST 2021"
```

## Profundizando en la conversión de fecha a cadena

Kotlin ofrece una gran variedad de patrones para formatear fechas, algunos de los más utilizados son:

- `EEE`: nombre corto del día de la semana (Ej: Mon, Tue, Wed)
- `EEE`: nombre largo del día de la semana (Ej: Monday, Tuesday, Wednesday)
- `dd`: día del mes con dos dígitos (Ej: 01, 02, 03)
- `MM`: mes con dos dígitos (Ej: 01, 02, 03)
- `MMMM`: nombre completo del mes (Ej: January, February, March)
- `yyyy`: año con cuatro dígitos (Ej: 2021, 2022, 2023)
- `hh`: hora en formato de 12 horas con dos dígitos (Ej: 01, 02, 03)
- `HH`: hora en formato de 24 horas con dos dígitos (Ej: 01, 02, 03)
- `mm`: minutos con dos dígitos (Ej: 01, 02, 03)
- `ss`: segundos con dos dígitos (Ej: 01, 02, 03)

Es importante mencionar que la clase `SimpleDateFormat` también permite el uso de caracteres especiales para formatear fechas personalizadas según nuestras necesidades.

## Ver también

- [Documentación oficial de Kotlin sobre fechas y tiempos](https://kotlinlang.org/docs/datetime.html)
- [Tutoriales de programación en Kotlin](https://www.tutorialesprogramacionya.com/kotlintercerprograma/)
- [Ejemplos de proyectos en Kotlin en GitHub](https://github.com/KotlinBy/awesome-kotlin)