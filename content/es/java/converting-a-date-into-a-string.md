---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "Java: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La conversión de una fecha a una cadena de texto es un proceso común en la programación en Java. Consiste en transformar una fecha en un formato legible para los humanos, como "dd/mm/aaaa", en una cadena de texto que puede ser almacenada o mostrada en una interfaz gráfica. Los programadores hacen esto para facilitar la manipulación y visualización de las fechas en sus aplicaciones.

## Cómo hacerlo:

```Java
import java.time.LocalDate; // Importa la clase LocalDate

LocalDate fecha = LocalDate.now(); // Crea una instancia de la clase LocalDate con la fecha actual

String fechaString = fecha.toString(); // Convierte la fecha en una cadena de texto en formato "aaaa-mm-dd"

System.out.println(fechaString); // Imprime la fecha convertida en la consola
```

Resultado de salida: "2021-09-01"

## Buceo profundo:

La conversión de fechas en cadenas de texto ha sido un problema recurrente en la programación desde los inicios del lenguaje Java. Originalmente, se utilizaba la clase Date para representar fechas, pero no tenía un método específico para convertirlas en cadenas de texto. Luego, se introdujo la clase Calendar que sí contaba con dicho método, pero aún presentaba algunas limitaciones. Finalmente, con la llegada de Java 8 se introdujo la clase LocalDate que simplificó en gran medida la conversión de fechas en cadenas de texto.

Existen diferentes formas de convertir una fecha en una cadena de texto en Java, como utilizando la clase SimpleDateFormat o la API de fecha y hora introducida en Java 8. También se pueden utilizar librerías externas como Joda-Time para realizar esta conversión. Sin embargo, la forma más sencilla y efectiva es utilizando la clase LocalDate como se muestra en el ejemplo anterior.

## Ver también:

- [Convertir una cadena de texto en una fecha en Java](https://www.freecodecamp.org/news/how-to-convert-string-to-datetime-in-java/)
- [Documentación de la clase LocalDate en Java](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Librería Joda-Time para manipulación de fechas en Java](https://www.joda.org/joda-time/)