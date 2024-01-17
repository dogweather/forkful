---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Java: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué se calcula una fecha en el futuro o pasado?
Calcular una fecha en el futuro o pasado es una función común en la programación en Java. Consiste en manipular fechas y horas para obtener una fecha específica, ya sea en el futuro o en el pasado. Los programadores suelen hacer esto para realizar tareas como programar eventos, recordatorios o realizar cálculos basados en ciertas fechas.

## Cómo hacerlo:
Para calcular una fecha en el futuro o pasado en Java, se pueden seguir los siguientes pasos:

- Importar la clase `java.time.LocalDate`: ```Java
import java.time.LocalDate;
```
- Crear un objeto de tipo `LocalDate` con la fecha actual:
```Java
LocalDate today = LocalDate.now();
```
- Utilizar el método `plus()` para sumar días, meses o años a la fecha actual: ```Java
LocalDate result = today.plus(10, ChronoUnit.DAYS);
```
- Utilizar el método `minus()` para restar días, meses o años a la fecha actual: ```Java
LocalDate result = today.minus(10, ChronoUnit.YEARS);
```
- Imprimir el resultado: ```Java
System.out.println("Resultado: " + result);
```

El resultado final sería una fecha en el futuro o pasado, dependiendo de los parámetros que se hayan utilizado.

## En detalle:
La manipulación de fechas y horas en Java se ha vuelto más fácil desde la introducción de la clase `java.time` en la versión 8. Antes de eso, se utilizaban las clases `Date` y `Calendar` que no eran tan eficientes ni fáciles de usar. La clase `LocalDate` ofrece muchos métodos útiles para manipular fechas, como `plus()`, `minus()`, `isBefore()` y `isAfter()`, entre otros.

Otra forma de calcular una fecha en el futuro o pasado es utilizando la clase `java.util.Calendar` y su método `add()`. Sin embargo, esta forma es más verbosa y menos eficiente en comparación con el uso de la clase `LocalDate`.

Es importante mencionar que al trabajar con fechas y horas, siempre se deben tener en cuenta posibles errores de formato o de zona horaria. Por lo tanto, es recomendable utilizar clases como `DateTimeFormatter` o `ZoneId` para garantizar la precisión y evitar problemas en el futuro.

## Ver también:
- [API de Java 8 - Clase LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutorial de manipulación de fechas en Java](https://www.baeldung.com/java-date-time-manipulation)