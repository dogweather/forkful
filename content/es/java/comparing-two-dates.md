---
title:                "Java: Comparando dos fechas"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué
En la programación, a menudo necesitamos comparar fechas para realizar diferentes tareas, como calcular la edad de una persona o determinar si una tarea está vencida. Por lo tanto, es importante comprender cómo comparar dos fechas en Java para poder escribir código efectivo y eficiente.

## Cómo hacerlo
Para comparar dos fechas en Java, podemos usar la clase `LocalDate` de la API de Java 8. Esta clase nos permite representar una fecha específica, sin tener en cuenta la hora o la zona horaria. Vamos a ver un ejemplo de cómo usar esta clase:

```Java
LocalDate fecha1 = LocalDate.of(2020, 10, 23);
LocalDate fecha2 = LocalDate.of(2021, 1, 15);
```

En este ejemplo, hemos creado dos objetos `LocalDate` que representan dos fechas diferentes: 23 de Octubre de 2020 y 15 de Enero de 2021.

Luego, podemos comparar estas dos fechas usando el método `compareTo()` de la clase `LocalDate`:

```Java
int comparacion = fecha1.compareTo(fecha2);
```

Este método devuelve un número entero que indica si la fecha1 es anterior, igual o posterior a la fecha2. Si el resultado es negativo, entonces fecha1 es anterior a fecha2. Si el resultado es positivo, entonces fecha1 es posterior a fecha2. Y si el resultado es cero, entonces fecha1 es igual a fecha2.

También podemos usar los métodos `isBefore()` y `isAfter()` para comparar fechas de manera más sencilla. Ambos métodos devuelven un valor booleano que indica si la fecha es antes o después de la otra, respectivamente.

Además de comparar fechas completas, también podemos comparar partes específicas de una fecha, como el año, el mes o el día. Para hacer esto, podemos usar los métodos `getYear()`, `getMonth()` y `getDayOfMonth()` respectivamente, y luego comparar los valores obtenidos.

## Profundizando
Además de la clase `LocalDate`, la API de Java 8 también cuenta con la clase `LocalDateTime` que nos permite representar una fecha y una hora específicas, y la clase `ZonedDateTime` que nos permite trabajar con fechas en diferentes zonas horarias.

También podemos usar la clase `Calendar` de versiones anteriores de Java para comparar fechas, pero es importante tener en cuenta que esta clase es menos eficiente y más propensa a errores.

## Ver también
- [Documentación de la clase LocalDate en Java](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutorial de comparación de fechas en Java](https://www.baeldung.com/java-compare-dates)
- [Video tutorial sobre cómo comparar fechas en Java](https://www.youtube.com/watch?v=kMfTh06tB9w)