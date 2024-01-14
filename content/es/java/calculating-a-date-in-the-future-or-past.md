---
title:    "Java: Calculando una fecha en el futuro o pasado"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por qué

A veces, como desarrolladores, nos enfrentamos a situaciones en las que necesitamos calcular una fecha en el futuro o en el pasado. Ya sea para programar una tarea o simplemente por curiosidad, la capacidad de realizar cálculos con fechas es una habilidad útil para tener en nuestro repertorio de habilidades de programación.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado en Java, podemos utilizar la clase `Calendar` y su método `add()`. Este método nos permite agregar o restar una cantidad específica de años, meses o días a una fecha determinada.

Por ejemplo, si queremos calcular la fecha de hoy en 1 año, podemos hacerlo de la siguiente manera:

```java
Calendar today = Calendar.getInstance();
today.add(Calendar.YEAR, 1);
```

El método `getInstance()` nos devuelve una instancia de la fecha actual en un objeto `Calendar`. Luego, utilizamos el método `add()` para agregar 1 año a esa fecha.

También podemos calcular una fecha en el pasado mediante el uso de un número negativo. Por ejemplo, si queremos saber cómo era la fecha de hoy hace 6 meses, podemos hacer lo siguiente:

```java
Calendar today = Calendar.getInstance();
today.add(Calendar.MONTH, -6);
```

El método `add()` también se puede utilizar para sumar o restar meses y días, simplemente cambiando el campo correspondiente en el primer parámetro.

Una vez que hemos realizado el cálculo, podemos obtener la fecha resultante utilizando el método `getTime()` y guardarla en una variable de tipo `Date`.

```java
Date futureDate = today.getTime();
```

Finalmente, podemos imprimir la fecha resultante utilizando un formato específico con la clase `SimpleDateFormat`.

```java
SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
System.out.println("La fecha dentro de un año será: " + dateFormat.format(futureDate));
```

La salida del código anterior sería algo como esto: `La fecha dentro de un año será: 18/06/2022`.

## Profundizando

El método `add()` de la clase `Calendar` es una forma sencilla de realizar cálculos con fechas en Java, pero también hay otras formas de hacerlo.

Una alternativa es utilizar la clase `LocalDate` de la librería `java.time` introducida en Java 8. Esta clase nos permite realizar operaciones similares con fechas de una manera más sencilla y legible.

Por ejemplo, para calcular la fecha de hoy en 1 año, podemos utilizar el método `plusYears()` de la siguiente manera:

```java
LocalDate today = LocalDate.now();
LocalDate futureDate = today.plusYears(1);
```

Para obtener la fecha resultante en un formato específico, podemos usar el método `format()` de la clase `DateTimeFormatter`:

```java
DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern("dd/MM/yyyy");
System.out.println("La fecha dentro de un año será: " + dateFormat.format(futureDate));
```

Para calcular fechas en el pasado, podemos usar el método `minus()` de la clase `LocalDate` de manera similar.

## Ver también

- [Documentación de la clase Calendar en Java](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Calendar.html)
- [Documentación de la clase LocalDate en Java](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html)
- [Tutorial de fechas y horas en Java](https://www.baeldung.com/java-date-time-api)