---
title:                "Java: Obteniendo la fecha actual"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué
En la programación, es muy común necesitar la fecha actual para realizar diferentes tareas. Ya sea para llevar un registro de la última vez que se realizó una acción, para mostrar información actualizada a los usuarios o para crear un archivo con la fecha actual en su nombre, la obtención de la fecha actual es un paso importante en muchas aplicaciones.

## Cómo
La forma más sencilla de obtener la fecha actual en Java es utilizando la clase `java.util.Date`. A continuación se muestra un ejemplo de código que imprime la fecha y hora actuales:

```Java
import java.util.Date;

public class FechaActual {
    public static void main(String[] args) {
        Date fecha = new Date();
        System.out.println("La fecha y hora actuales son: " + fecha);
    }
}
```

Al ejecutar este código, se obtendrá una salida similar a la siguiente:

```
La fecha y hora actuales son: Sun Jun 13 21:04:29 CEST 2021
```

Sin embargo, esta clase tiene algunas limitaciones, por lo que es recomendable utilizar la clase `java.time.LocalDateTime`, introducida en Java 8. Esta clase permite obtener una fecha y hora más precisa, así como realizar operaciones con ellas. A continuación se muestra un ejemplo de cómo imprimir la fecha y hora actuales utilizando esta clase:

```Java
import java.time.LocalDateTime;

public class FechaActual {
    public static void main(String[] args) {
        LocalDateTime fecha = LocalDateTime.now();
        System.out.println("La fecha y hora actuales son: " + fecha);
    }
}
```

La salida de este código será similar a la siguiente:

```
La fecha y hora actuales son: 2021-06-13T21:04:29.920
```

## Deep Dive
La clase `java.time.LocalDateTime` pertenece al paquete `java.time`, el cual ofrece una amplia gama de clases para manejar fechas, horas e incluso zonas horarias en Java. Estas clases son inmutables, lo que significa que no se pueden modificar una vez creadas. Además, también se pueden realizar operaciones matemáticas y de comparación con ellas, lo que las hace muy versátiles.

Algunos métodos útiles que ofrece la clase `LocalDateTime` son `now()` para obtener la fecha y hora actuales, `of()` para crear una fecha y hora específica, `plus()` y `minus()` para sumar o restar tiempo, y `isBefore()` e `isAfter()` para comparar fechas y horas. Puedes explorar más sobre estas clases y sus métodos en la documentación oficial de Java.

## Ver también
- [Documentación oficial de Java sobre fechas y horas](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Tutorial de Java sobre fechas y horas](https://www.tutorialspoint.com/java8/java8_datetime_api.htm)
- [Ejemplos prácticos de uso de fechas y horas en Java](https://examples.javacodegeeks.com/core-java/util/date/java-util-date-example/)

¡Espero que este artículo te haya sido útil para entender cómo obtener la fecha actual en tus proyectos de Java! Recuerda que es importante manejar correctamente las fechas y horas en tus aplicaciones para evitar errores y confusiones. ¡Hasta la próxima!