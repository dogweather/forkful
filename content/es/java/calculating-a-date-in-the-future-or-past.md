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

## Por qué

Calcular una fecha en el futuro o en el pasado puede ser útil para realizar tareas como planificar eventos, realizar seguimiento de plazos o calcular la edad de una persona en una fecha determinada.

## Cómo hacerlo

Para calcular una fecha en Java, primero se debe utilizar la clase `LocalDate` del paquete `java.time`. A continuación, se pueden utilizar diferentes métodos para manipular la fecha, como `plusDays()` para sumar días, `plusMonths()` para sumar meses o `plusYears()` para sumar años.

```Java
// Importar la clase LocalDate
import java.time.LocalDate;

// Crear una fecha de ejemplo
LocalDate fecha = LocalDate.of(2020, 10, 15);

// Sumar 5 días a la fecha
LocalDate fechaFutura = fecha.plusDays(5); // Resultado: 2020-10-20

// Restar 2 meses a la fecha
LocalDate fechaPasada = fecha.minusMonths(2); // Resultado: 2020-08-15

// Calcular la edad en una fecha determinada
LocalDate fechaNacimiento = LocalDate.of(1990, 5, 25);
LocalDate fechaActual = LocalDate.now();
int edad = fechaNacimiento.until(fechaActual).getYears(); // Resultado: 30
```

## Profundizando

Java ofrece varias clases y métodos para trabajar con fechas y realizar cálculos. Algunas de las clases más importantes son `LocalDate`, `LocalDateTime` y `ZonedDateTime`. Además, se pueden utilizar métodos como `until()` para obtener la diferencia entre dos fechas, `isBefore()` y `isAfter()` para comparar fechas, y `get()` para obtener valores específicos como el día o el mes.

Es importante tener en cuenta que las fechas en Java son inmutables, lo que significa que no se pueden modificar directamente. En su lugar, se deben crear nuevas instancias con los valores deseados.

## Ver también

- Documentación oficial de Java sobre el paquete `java.time`: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- Tutorial de Baeldung sobre cómo trabajar con fechas en Java: https://www.baeldung.com/java-date-time
- Ejemplos de código para calcular fechas en Java: https://www.baeldung.com/java-math-dates