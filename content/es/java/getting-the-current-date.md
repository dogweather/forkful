---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Obtener la fecha actual en programación permite rastrear eventos en tiempo real. Los programadores lo utilizan para funciones de registro, marcar transacciones, o incluso programar tareas.

## Cómo hacerlo:
En Java puedes conseguir la fecha actual utilizando la clase `java.time.LocalDate'. Aquí está el ejemplo:

```Java
import java.time.LocalDate;

public class Main {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println("La fecha actual es: " + currentDate);
    }
}
```
El resultado será algo similar a: `La fecha actual es: 2022-09-21`

## Inmersión Profunda
Al inicio, los programadores de Java utilizaban la clase `java.util.Date` para obtener la fecha actual, pero con la introducción de Java 8, la clase `java.time.LocalDate` se convirtió en la forma preferida debido a su simplicidad y eficacia.

Existen opciones alternativas, como `java.util.Calendar` que ofrece una variedad de métodos para manipular fechas, sin embargo, es más pesado y complicado en comparación con `LocalDate`.

Obtener la fecha actual no es un proceso tremendamente difícil en Java. `LocalDate.now()` internamente utiliza el reloj del sistema para obtener la fecha actual, convirtiéndolo en una herramienta apropiada para usos comúnmente necesitados.

## Ver También:
Para mayor comprensión, puedes referirte a estas fuentes:

- Documentación oficial Java para LocalDate: [aquí](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html) 

- Un tutorial útil en inglés para manejar fechas y tiempo en Java: [aquí](https://www.baeldung.com/java-8-date-time-intro)

- Un artículo detallado sobre la evolución de la manipulación de la fecha y hora en Java: [aquí](https://www.journaldev.com/2800/java-8-date-localdate-localdatetime-instant)