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

## Por qué

Hay muchas razones por las que un programador puede necesitar convertir una fecha en una cadena de texto en Java. Puede ser necesario para mostrar la fecha en un formato específico, almacenar la fecha en una base de datos o para realizar operaciones matemáticas con la fecha. En cualquier caso, saber cómo convertir una fecha en una cadena de texto es una habilidad importante para cualquier desarrollador de Java.

## Cómo hacerlo

La conversión de una fecha en una cadena de texto en Java se puede lograr utilizando la clase `SimpleDateFormat`. Primero, se crea un objeto de esta clase y se especifica el formato de fecha deseado. Luego, se utiliza el método `format()` para convertir la fecha en una cadena de texto según el formato especificado. A continuación se muestra un ejemplo de código utilizando esta técnica:

```java
// Crear objeto SimpleDateFormat con formato "dd/MM/yyyy"
SimpleDateFormat formatoFecha = new SimpleDateFormat("dd/MM/yyyy");

// Obtener la fecha actual
Date fechaActual = new Date();

// Convertir la fecha actual en una cadena de texto
String fechaConvertida = formatoFecha.format(fechaActual);

// Imprimir la cadena de texto resultante
System.out.println("La fecha actual es: " + fechaConvertida); // Salida: "La fecha actual es: 26/03/2021"
```

El formato de fecha utilizado en el ejemplo es solo uno de los muchos posibles. Puedes consultar la documentación oficial de la clase `SimpleDateFormat` para ver todos los patrones de formato disponibles.

## Profundizando

Además de la clase `SimpleDateFormat`, Java también cuenta con la clase `DateTimeFormatter` introducida en la versión 8. Esta clase proporciona una forma más avanzada y flexible de convertir una fecha en una cadena de texto. Un ejemplo de uso de `DateTimeFormatter` sería el siguiente:

```java
// Crear un objeto DateTimeFormatter con formato "EE, MMM dd yyyy"
DateTimeFormatter formatoFecha = DateTimeFormatter.ofPattern("EE, MMM dd yyyy");

// Obtener la fecha actual
LocalDateTime fechaActual = LocalDateTime.now();

// Convertir la fecha actual en una cadena de texto
String fechaConvertida = fechaActual.format(formatoFecha);

// Imprimir la cadena de texto resultante
System.out.println("La fecha actual es: " + fechaConvertida); // Salida: "La fecha actual es: vie, mar 26 2021"
```

Además, el método `format()` de `DateTimeFormatter` también puede aceptar un `Locale` como parámetro para mostrar la fecha en diferentes idiomas y formatos, lo que lo hace aún más versátil.

## Ver también

- [Documentación oficial de la clase SimpleDateFormat] (https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Documentación oficial de la clase DateTimeFormatter] (https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)