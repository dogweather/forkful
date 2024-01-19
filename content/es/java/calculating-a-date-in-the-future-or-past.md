---
title:                "Calculando una fecha en el futuro o en el pasado"
html_title:           "Java: Calculando una fecha en el futuro o en el pasado"
simple_title:         "Calculando una fecha en el futuro o en el pasado"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Calculando una Fecha en el Futuro o en el Pasado en Java 

## ¿Qué & Por qué?

Calcular una fecha en el futuro o en el pasado implica manipular datos de fecha y hora para obtener una nueva fecha. Los programadores hacen esto para gestionar tareas como temporizadores, recordatorios y eventos recurrentes.   

## Cómo hacerlo:

En Java, uno puede utilizar la clase `LocalDate` para hacerlo.

```Java
import java.time.*;

class FechaFuturaOPasada {
   public static void main(String args[]){
      LocalDate hoy = LocalDate.now();
      LocalDate futuro = hoy.plusDays(10);
      LocalDate pasado = hoy.minusDays(10);

      System.out.println("Fecha actual: " + hoy);
      System.out.println("Fecha 10 días en el futuro: " + futuro);
      System.out.println("Fecha 10 días en el pasado: " + pasado);
   }
}
```

La salida será similar a esto:

```Java
Fecha actual: 2022-05-12
Fecha 10 días en el futuro: 2022-05-22
Fecha 10 días en el pasado: 2022-05-02
```

## Análisis a Fondo:

Historicamente, antes de Java 8, los programadores usaban `java.util.Date` y `java.util.Calendar`, los cuales son difíciles de usar y propensos a errores. La aparición de `java.time` en Java 8 revolucionó completamente este enfoque.

Las alternativas a calcular las fechas podrían incluir el uso de bibliotecas de terceros como Joda-Time.

Desde el punto de vista de la implementación, la nueva API de fecha y hora en Java se basa en el estándar ISO 8601, que es el estándar de facto para la representación de la fecha y la hora en los sistemas informáticos.

## Ver También:

- Documentación oficial de Java para LocalDate: [https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- Documentación oficial de Java para la clase `java.time`: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- Biblioteca Joda-Time: [https://www.joda.org/joda-time/](https://www.joda.org/joda-time/)