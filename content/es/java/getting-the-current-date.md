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

## ¿Qué y por qué?
Obtener la fecha actual es un proceso que permite a los programadores obtener la fecha y hora exactas en que se está ejecutando un programa. Esto es útil para realizar operaciones con fechas y marcar eventos en el código. 

## ¿Cómo hacerlo?
Puedes obtener la fecha actual utilizando la clase `LocalDate` de la librería `java.time`. Aquí tienes un ejemplo de código para obtener la fecha actual en formato `dd/mm/yyyy`:

```Java
LocalDate currentDate = LocalDate.now();
System.out.println(currentDate.format(DateTimeFormatter.ofPattern("dd/MM/yyyy")));
```
La salida de este código sería: `16/10/2021` (la fecha actual en el momento de la ejecución).

## Profundizando
Esta funcionalidad ha evolucionado a lo largo del tiempo, con diferentes implementaciones en distintos lenguajes de programación. Antes de las librerías `java.time` en Java, se utilizaba la clase `Date` de la librería `java.util`, pero esta ha sido ampliamente reemplazada por la clase `LocalDate`.

Existen también otras opciones para obtener la fecha actual, como utilizar una API en línea o incluso obtenerla directamente del sistema operativo. Sin embargo, la forma más recomendada actualmente es utilizar la clase `LocalDate`.

## Más información
- Java `java.time.LocalDate` documentation: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- Alternativas a la clase `LocalDate` en Java: https://www.baeldung.com/java-date-time-operations
- Obtener la fecha actual de forma precisa en Java: https://dzone.com/articles/6-ways-to-get-current-date-and-time-in-java