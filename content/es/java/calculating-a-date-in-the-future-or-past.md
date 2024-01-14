---
title:    "Java: Calculando una fecha en el futuro o pasado"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué:
Calcular una fecha en el futuro o en el pasado es una habilidad útil en la programación. Puede servir para planificar eventos, automatizar tareas y mucho más.

## Cómo hacerlo:
Para calcular una fecha en el futuro o en el pasado, debemos entender cómo funciona el sistema de fechas en Java y utilizar las clases y métodos adecuados. A continuación, se muestra un ejemplo de código para calcular una fecha 7 días en el futuro y mostrarla en el formato "dd/MM/yyyy":

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class CalculadoraDeFechas {

    public static void main(String[] args) {

        // Obtener la fecha actual
        LocalDate fechaActual = LocalDate.now();
        System.out.println("Fecha actual: " + fechaActual);

        // Calcular la fecha 7 días en el futuro
        LocalDate fechaFutura = fechaActual.plusDays(7);
        System.out.println("Fecha en 7 días: " + fechaFutura);

        // Formatear la fecha en el formato "dd/MM/yyyy"
        DateTimeFormatter formateador = DateTimeFormatter.ofPattern("dd/MM/yyyy");
        String fechaFormateada = fechaFutura.format(formateador);
        System.out.println("Fecha formateada: " + fechaFormateada);
    }
}
```
El resultado de ejecutar este código sería:
```
Fecha actual: 27/06/2021
Fecha en 7 días: 04/07/2021
Fecha formateada: 04/07/2021
```

## Profundizando:
Para calcular una fecha en el futuro o en el pasado, se utilizan las clases LocalDate y LocalDateTime de la librería java.time. Además, podemos utilizar otros métodos como plusDays() para sumar días a una fecha o minusDays() para restar días. También podemos formatear la fecha en diferentes formatos utilizando la clase DateTimeFormatter.

Ahora que conocemos cómo funciona el sistema de fechas en Java, podemos utilizar esta habilidad en proyectos más complejos y mejorar nuestra eficiencia en la programación.

## Ver también:
- [LocalDate documentación](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [DateTimeFormatter documentación](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Tutorial de Java en español](https://www.javatpoint.com/java-tutorial)