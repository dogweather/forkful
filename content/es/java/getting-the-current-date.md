---
title:                "Obteniendo la fecha actual"
html_title:           "Java: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Por qué alguien se interesaría en obtener la fecha actual en Java?

Obtener la fecha actual es una tarea común en la programación en Java. Puede ser útil para mostrar la fecha en una interfaz de usuario, validar fechas en una aplicación de gestión de datos, o incluso para procesar datos basados en la fecha actual.

## Cómo obtener la fecha actual en Java

Para obtener la fecha actual en Java, podemos utilizar la clase `java.util.Date` o la clase `java.time.LocalDate` introducida en Java 8. A continuación, se muestran ejemplos de código utilizando ambas opciones:

```java
import java.util.Date;
import java.time.LocalDate;

// Usando la clase java.util.Date
Date date = new Date();
System.out.println(date); // Output: Mon Jun 21 17:34:57 CST 2021

// Usando la clase java.time.LocalDate
LocalDate currentDate = LocalDate.now();
System.out.println(currentDate); // Output: 2021-06-21

```

Podemos observar que ambas opciones nos proporcionan la fecha actual en diferentes formatos. Además, también podemos utilizar la clase `java.time.LocalDateTime` para obtener la fecha y hora actual.

## Profundizando en la obtención de la fecha actual en Java

Si queremos ser más específicos en la obtención de la fecha actual en Java, podemos utilizar el patrón de diseño `DateTimeFormatter` para formatear la salida de la fecha. Además, también podemos utilizar la clase `java.util.Calendar` para obtener información específica como el mes, el día de la semana o el año actual. A continuación, se muestran ejemplos de código usando estas opciones:

```java
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Calendar;

// Utilizando el patrón de diseño DateTimeFormatter para formatear la fecha
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
LocalDateTime date = LocalDateTime.now();
String formattedDate = date.format(formatter);
System.out.println("Fecha actual formateada: " + formattedDate); // Output: Fecha actual formateada: 21/06/2021

// Utilizando la clase java.util.Calendar
Calendar calendar = Calendar.getInstance();
System.out.println("Día de la semana actual: " + calendar.get(Calendar.DAY_OF_WEEK)); // Output: Día de la semana actual: 2 (lunes)
```

Como podemos ver, hay múltiples opciones para obtener la fecha actual en Java, dependiendo de nuestras necesidades específicas.

## Ver también

- Documentación oficial de Java sobre la clase `java.util.Date` (https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- Documentación oficial de Java sobre la clase `java.time` (https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- Beginner's Guide to Java Date and Time API (https://www.baeldung.com/java-date-time)