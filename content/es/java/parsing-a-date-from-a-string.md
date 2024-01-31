---
title:                "Análisis de una fecha a partir de una cadena"
date:                  2024-01-20T15:37:00.947093-07:00
html_title:           "Arduino: Análisis de una fecha a partir de una cadena"
simple_title:         "Análisis de una fecha a partir de una cadena"

category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Parsear una fecha desde una cadena significa convertir texto que representa una fecha (como "01/04/2023") a un objeto de fecha en Java. Lo hacemos porque manipular fechas como objetos nos permite hacer todo tipo de operaciones con más facilidad, como comparaciones, cálculos de tiempo y formateo.

## Cómo hacerlo:
Para parsear una fecha en Java, utilizamos la clase `DateTimeFormatter` de la biblioteca `java.time`, seguido por una de las clases de fecha, como `LocalDate`. Aquí tienes un ejemplo claro:

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

public class FechaEjemplo {
    public static void main(String[] args) {
        String fechaTexto = "01/04/2023";
        DateTimeFormatter formato = DateTimeFormatter.ofPattern("dd/MM/yyyy");

        try {
            LocalDate fecha = LocalDate.parse(fechaTexto, formato);
            System.out.println(fecha);  // Output: 2023-04-01
        } catch (DateTimeParseException e) {
            System.out.println("Formato de fecha inválido.");
        }
    }
}
```

Si corres este código, obtendrás la fecha en formato ISO (aaaa-mm-dd).

## Profundizando
Antes de Java 8, la gente usaba `SimpleDateFormat` de `java.text`, que era menos intuitivo y tenía problemas con la seguridad de hilos. `DateTimeFormatter` resolvió estos problemas con un diseño inmutable y seguro para hilos.

Una alternativa es `DateFormat` de la vieja escuela, pero ya casi no se usa debido a sus problemas.

En la implementación, el parseo usa un patrón definido. Si el texto no coincide con el patrón o es inválido, lanza una `DateTimeParseException`. Esto está bueno porque atrapa errores temprano.

## Ver También
Puedes profundizar con estos enlaces:

- [Java 8 Date/Time guide](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
- La documentación oficial de Oracle sobre `DateTimeFormatter`: [DateTimeFormatter (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- Un tutorial sobre manejo de fechas y horas en Java: [Baeldung Date and Time](https://www.baeldung.com/java-8-date-time-intro)
