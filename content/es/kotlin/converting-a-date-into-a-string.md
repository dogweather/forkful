---
title:    "Kotlin: Convirtiendo una fecha en una cadena"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por qué

Una de las tareas más comunes en programación es trabajar con fechas y horas. A menudo, es necesario convertir una fecha en un formato legible para el usuario, como una cadena de texto. En esta publicación, exploraremos cómo hacer esto en Kotlin.

## Cómo hacerlo

Para convertir una fecha en una cadena en Kotlin, utilizaremos la función `format()` de la clase `LocalDateTime`. Primero, crearemos un objeto `LocalDateTime` con una fecha y hora específicas:

```Kotlin
val fecha = LocalDateTime.of(2021, 10, 15, 18, 30)
```

Luego, utilizando la función `format()`, especificaremos el formato deseado. Por ejemplo, si queremos que la fecha se muestre en formato "dd/MM/yyyy HH:mm", usaremos:

```Kotlin
val fechaCadena = fecha.format(DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm"))
```

Al imprimir `fechaCadena`, veremos el siguiente resultado:

```Kotlin
15/10/2021 18:30
```

También podemos aprovechar las funciones de extensión de la clase `LocalDateTime` para obtener resultados similares. Por ejemplo, para mostrar solo la hora y los minutos, podemos usar lo siguiente:

```Kotlin
val hora = fecha.format(DateTimeFormatter.ofPattern("HH:mm"))
```

Y obtendremos:

```Kotlin
18:30
```

## Profundizando

La función `format()` también acepta un objeto `DateTimeFormatter`. Podemos utilizar este objeto para personalizar aún más el formato de nuestra fecha. Por ejemplo, podemos especificar el idioma y el estilo de fecha deseado, como en el siguiente código:

```Kotlin
val fecha = LocalDateTime.of(2021, 10, 15, 18, 30)
val fechaCadena = fecha.format(
    DateTimeFormatter.ofLocalizedDate(FormatStyle.FULL).withLocale(Locale("es", "ES"))
)
```

Esto nos devolverá la fecha en formato "viernes 15 de octubre de 2021", en español.

También podemos utilizar la función `format()` para convertir una fecha en una cadena en formato ISO. Por ejemplo:

```Kotlin
val fecha = LocalDateTime.of(2021, 10, 15, 18, 30)
val fechaISO = fecha.format(DateTimeFormatter.ISO_DATE_TIME)
```

Esto nos dará la fecha en formato "2021-10-15T18:30:00".

## Ver también

Para obtener más información sobre cómo trabajar con fechas y horas en Kotlin, te recomendamos revisar estos recursos:

- [Documentación oficial de Java sobre fechas y horas](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Tutorial de Codecademy sobre fechas y horas en Kotlin](https://www.codecademy.com/learn/learn-kotlin/modules/learn-kotlin-date-and-time)

¡Esperamos que esta publicación te haya sido útil para convertir fechas en cadenas en Kotlin! ¡Nos vemos en la próxima!