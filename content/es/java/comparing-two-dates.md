---
title:    "Java: Comparando dos fechas"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Por qué comparar dos fechas en Java?

Comparar dos fechas es una tarea común en la programación en Java. Puede ser útil para realizar tareas como verificar si una fecha es anterior o posterior a otra, o para ordenar una lista de fechas. En este artículo, exploraremos cómo comparar dos fechas en Java y profundizaremos en los conceptos detrás de este proceso.

## Cómo hacerlo

Para comparar dos fechas en Java, existen varias opciones disponibles. Una de ellas es usar la clase `java.util.Date` y sus métodos `compareTo` o `before`/`after`. Por ejemplo:

```Java
Date fecha1 = new Date(2021, 3, 15);
Date fecha2 = new Date(2020, 2, 10);

int resultado = fecha1.compareTo(fecha2);
if (resultado == 0) {
    System.out.println("Las fechas son iguales");
} else if (resultado < 0) {
    System.out.println("La fecha1 es anterior a la fecha2");
} else {
    System.out.println("La fecha1 es posterior a la fecha2");
}
```

En este ejemplo, creamos dos objetos `Date` con diferentes fechas y luego usamos el método `compareTo` para compararlos. El método devuelve un valor entero que indica si la fecha actual es anterior, posterior o igual a la fecha con la que se compara.

También es posible utilizar la clase `java.time.LocalDate` introducida en Java 8 para comparar fechas. Por ejemplo:

```Java
LocalDate fecha1 = LocalDate.of(2021, 3, 15);
LocalDate fecha2 = LocalDate.of(2020, 2, 10);

int resultado = fecha1.compareTo(fecha2);
if (resultado == 0) {
    System.out.println("Las fechas son iguales");
} else if (resultado < 0) {
    System.out.println("La fecha1 es anterior a la fecha2");
} else {
    System.out.println("La fecha1 es posterior a la fecha2");
}
```

En este caso, utilizamos el método `compareTo` de la clase `LocalDate` para comparar las dos fechas. El resultado será el mismo que en el ejemplo anterior.

Otra opción es utilizar la clase `java.util.Calendar`, específicamente el método `compareTo` de su subclase `GregorianCalendar`. Por ejemplo:

```Java
Calendar fecha1 = new GregorianCalendar(2021, 3, 15);
Calendar fecha2 = new GregorianCalendar(2020, 2, 10);

int resultado = fecha1.compareTo(fecha2);
if (resultado == 0) {
    System.out.println("Las fechas son iguales");
} else if (resultado < 0) {
    System.out.println("La fecha1 es anterior a la fecha2");
} else {
    System.out.println("La fecha1 es posterior a la fecha2");
}
```

De nuevo, se utiliza el método `compareTo` para comparar las dos fechas. Tenga en cuenta que en este caso se utilizan valores numéricos para representar el mes, ya que en `Calendar` los meses empiezan en 0 (enero) en lugar de 1 como en las otras clases mencionadas anteriormente.

## Profundizando

A la hora de comparar dos fechas, es importante tener en cuenta cómo se comparan los objetos. En el caso de la clase `java.util.Date`, los objetos se comparan en función de su valor numérico, lo que significa que se tienen en cuenta no solo la fecha, sino también la hora y el minuto. Por otro lado, en la clase `java.time.LocalDate`, los objetos se comparan solo por su fecha, omitiendo la hora y el minuto.

Además, es importante señalar que en la clase `java.util.Calendar`, el método `compareTo` no compara solo las fechas, sino todas las unidades de tiempo (mes, día, hora, minuto, etc.). Esto significa que es posible que dos fechas no sean exactamente iguales en términos de valor numérico, pero aún así sean consideradas iguales.

## Ver también

Si quieres profundizar más en la comparación de fechas en Java, te recomendamos revisar los siguientes enlaces:

- [Documentación oficial de Java sobre la clase Date](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Documentación oficial de Java sobre la clase LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Documentación oficial de Java sobre la clase Calendar](https://docs.oracle.com/javase/8