---
title:                "Java: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Por qué calcular fechas en el futuro o pasado?

Calcular fechas en el futuro o pasado puede ser útil en una variedad de situaciones de programación, como por ejemplo para planificar eventos, realizar proyecciones o simplemente para obtener información precisa.

## Cómo hacerlo

Para calcular una fecha en el futuro o pasado en Java, podemos utilizar la clase `LocalDate` de la biblioteca estándar `java.time`. Primero, debemos importar la clase en nuestro código:

```Java
import java.time.LocalDate;
```

A continuación, podemos crear un objeto `LocalDate` con la fecha actual utilizando el método `now()` y luego utilizar el método `plusDays()` o `minusDays()` para agregar o restar días, respectivamente. Por ejemplo, si queremos calcular la fecha dentro de dos semanas en el futuro, podemos hacer lo siguiente:

```Java
LocalDate fechaActual = LocalDate.now();
LocalDate fechaFutura = fechaActual.plusDays(14);
```

También podemos utilizar métodos como `plusMonths()`, `plusYears()` o incluso `plus()` para calcular fechas en el futuro o pasado en meses o años. Para obtener la fecha como una cadena de texto en un formato específico, podemos utilizar el método `format()`. Por ejemplo:

```Java
String fechaFormateada = fechaFutura.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"));
System.out.println(fechaFormateada); // Output: 16/08/2021
```

## Profundizando en el tema

Además de los métodos mencionados anteriormente, la clase `LocalDate` nos ofrece muchas otras opciones para calcular fechas en el futuro o pasado. Por ejemplo, podemos utilizar métodos como `with()`, `minusMonths()` o `minusYears()` para modificar una fecha determinada, o utilizar `isBefore()` y `isAfter()` para comparar fechas.

También podemos realizar cálculos más complejos, como calcular la cantidad de días entre dos fechas, utilizando el método `until()`. Todo esto y más se puede encontrar en la documentación oficial de la clase `LocalDate`.

## Ver también

- [Documentación oficial de LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutorial de JavaFX: cómo manejar fechas con LocalDate](https://code.makery.ch/library/javafx-tutorial/part7/)