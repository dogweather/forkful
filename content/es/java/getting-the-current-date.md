---
title:                "Obteniendo la fecha actual"
date:                  2024-01-20T15:15:33.450057-07:00
html_title:           "Bash: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Obtener la fecha actual en Java es simplemente capturar el momento presente del sistema. Los programadores lo hacen para registros, marcas de tiempo, y funciones que dependen de la fecha y hora actuales.

## Cómo:

La forma más moderna y recomendada de obtener la fecha actual:

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class FechaActual {
    public static void main(String[] args) {
        LocalDate fechaHoy = LocalDate.now();
        DateTimeFormatter formato = DateTimeFormatter.ofPattern("dd/MM/yyyy");
        String fechaFormateada = fechaHoy.format(formato);
        
        System.out.println("Fecha actual: " + fechaFormateada);
    }
}
```

Ejecutando el código anterior, podría obtener un resultado parecido a:

```
Fecha actual: 05/04/2023
```

## Profundización:

Históricamente, antes de Java 8, `java.util.Date` y `java.util.Calendar` eran las clases comunes para manejar fechas, pero no eran sin sus problemas, como la mutabilidad y la pobre diseño de API. Desde Java 8 en adelante, se introdujo `java.time` (conocido como JSR-310), una API robusta y a prueba de fallos diseñada por los expertos en tiempo Joda-Time.

Alternativas antiguas y por qué no usarlas:

- `java.util.Date`: Es mutable y su uso en un contexto complejo podría llevar a errores. Además, no maneja bien los husos horarios.
- `java.util.Calendar`: Más poderosa que Date, pero sigue siendo mutable y un poco complicada de usar.

Detalles de implementación actuales:

- `java.time.LocalDate`: Representa una fecha sin información de hora o huso horario. Perfecta para cuando solo necesitas la fecha.
- `java.time.LocalDateTime`: Fecha y hora sin huso horario.
- `java.time.ZonedDateTime`: Fecha y hora con información de huso horario.

La API de tiempo de Java es inmutable, lo que significa que cualquier operación devuelve una nueva instancia, preservando la original. Esto es excelente para prevenir errores comunes de programación.

## Ver Además:

Para más detalles sobre el trabajo con fechas y horas en Java:

- [Date and Time API Guide - Oracle](https://docs.oracle.com/javase/tutorial/datetime/)
- [Java 8 Date Time API - Baeldung](https://www.baeldung.com/java-8-date-time-intro)
- [Understanding Java 8 Date and Time API - Medium](https://medium.com/@randerson112358/understand-java-8-date-time-api-ee8377059cfd)

Si necesitas un repaso de por qué es importante manejar correctamente la inmutabilidad en programación:

- [Inmutabilidad en Programación - Java Deep](https://deepinjava.wordpress.com/2017/09/29/inmutabilidad-en-java/)
