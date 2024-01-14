---
title:                "Java: Calculando una fecha en el futuro o pasado"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

Calcular una fecha en el futuro o en el pasado puede ser una tarea muy útil en muchas situaciones diferentes. Puede usar este tipo de cálculo para planear eventos, recordar fechas importantes o simplemente para tener una mejor comprensión del paso del tiempo.

## Cómo hacerlo

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class CalculadorDeFechas {

    public static void main(String[] args) {

        // Calculando la fecha 10 días después de hoy
        LocalDate fechaFutura = LocalDate.now().plusDays(10);
        System.out.println("Fecha futura: " + fechaFutura);

        // Calculando la fecha 2 años en el pasado
        LocalDate fechaPasada = LocalDate.now().minusYears(2);
        System.out.println("Fecha pasada: " + fechaPasada);

        // Convirtiendo una fecha de string a objeto LocalDate
        String fechaString = "25/05/2022";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
        LocalDate fechaEspecifica = LocalDate.parse(fechaString, formatter);
        System.out.println("Fecha específica: " + fechaEspecifica);
    }
}

```

**Salida:**

```
Fecha futura: 2021-09-27
Fecha pasada: 2019-09-27
Fecha específica: 2022-05-25
```

## Profundizando

La clase `LocalDate` en Java proporciona métodos útiles para trabajar con fechas. Con el método `plusDays()` podemos agregar días a una fecha dada, y con el método `minusYears()` podemos restar años. Además, podemos usar el método `parse()` y especificar un formato para convertir una fecha de string a un objeto `LocalDate`.

Es importante tener en cuenta que el método `plusDays()` y `minusYears()` devuelven un nuevo objeto `LocalDate`, ya que las fechas en Java son inmutables. Esto significa que no podemos modificar un objeto `LocalDate` existente, sino que siempre crearemos uno nuevo con la fecha modificada.

## Ver también

- [Documentación de la clase LocalDate en Java](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutorial de Java: Trabajando con fechas](https://www.javatpoint.com/java-localdate)
- [Cómo calcular una fecha en Java](https://stackabuse.com/how-to-calculate-the-date-in-java/)