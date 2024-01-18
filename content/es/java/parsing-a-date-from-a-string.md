---
title:                "Analizando una fecha desde una cadena"
html_title:           "Java: Analizando una fecha desde una cadena"
simple_title:         "Analizando una fecha desde una cadena"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

El proceso de "parsing" (análisis sintáctico) de una fecha a partir de una cadena de caracteres es algo que los programadores hacen con frecuencia. Básicamente, se trata de convertir una fecha en un formato legible para la computadora, como una cadena de texto, a un objeto de fecha en el lenguaje de programación. Esto es importante para tareas como cálculos de tiempo, almacenamiento de fechas en bases de datos y cualquier otra operación que involucre fechas.

## ¿Cómo?

```Java
// Ejemplo básico de parsing de una fecha en Java
String fecha = "12-05-2021"; // cadena de caracteres con la fecha
DateFormat formato = new SimpleDateFormat("dd-MM-yyyy"); // formato deseado
Date fechaObjeto = formato.parse(fecha); // se realiza el análisis sintáctico
System.out.println(fechaObjeto); // imprime la fecha en formato objeto

// Salida: Wed May 12 00:00:00 CEST 2021
```

## Profundizando

El análisis sintáctico de fechas ha existido desde los primeros lenguajes de programación. En Java, la clase `DateFormat` maneja el proceso de parsing, pero también existen otras opciones como `DateTimeFormatter` en Java 8, que ofrece un mejor manejo de fechas y tiempos. Además, algunos frameworks populares como Spring y Joda-Time también tienen sus propias implementaciones de parsing de fechas.

## Ver también

- [Documentación de la clase DateFormat en la plataforma Java](https://docs.oracle.com/javase/8/docs/api/java/text/DateFormat.html)
- [Tutorial de Java Date and Time API](https://www.baeldung.com/java-date-time)
- [Documentación oficial de Joda-Time](https://www.joda.org/joda-time/)
- [Documentación oficial de Spring Framework](https://spring.io/projects/spring-framework)