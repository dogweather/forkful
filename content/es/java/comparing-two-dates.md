---
title:                "Comparando dos fechas"
html_title:           "Java: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Comparar dos fechas es una tarea común en la programación, que consiste en verificar qué fecha es anterior o posterior a otra. Los programadores lo hacen para realizar tareas como ordenar una lista de fechas o comprobar si una fecha está dentro de un rango determinado.

## ¡Cómo hacerlo!
```Java
// Declaración de dos objetos de tipo Date
Date fecha1 = new Date();
Date fecha2 = new Date();
// Comparación usando el método compareTo()
int resultado = fecha1.compareTo(fecha2);
// Imprime el resultado de la comparación
System.out.println(resultado);
```
**Salida:**
- Si fecha1 es anterior a fecha2, el resultado será un número negativo.
- Si fecha1 es igual a fecha2, el resultado será 0.
- Si fecha1 es posterior a fecha2, el resultado será un número positivo.

## Inmersión profunda
### Contexto histórico
Antes de la introducción de la clase Date en Java, los programadores tenían que realizar manualmente la comparación de fechas utilizando cálculos y conversiones de formatos de fechas. La clase Date facilita este proceso al proporcionar métodos como compareTo() y before().

### Alternativas
Además de la clase Date, también existen otras opciones para comparar fechas en Java, como la clase Calendar y la clase LocalDate de la librería Java 8. Estas clases ofrecen más funcionalidades y flexibilidad para trabajar con fechas, pero también requieren un poco más de conocimiento para utilizarlas correctamente.

### Detalles de implementación
La comparación de fechas en Java se basa en la representación interna de las fechas en milisegundos desde el 1 de enero de 1970. El método compareTo() compara estas representaciones internas de las fechas, por lo que es importante asegurarse de que las fechas estén en el mismo formato antes de la comparación.

## Véase también
- [Documentación oficial de la clase Date en Java](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Tutorial de Java Calendar class](https://www.baeldung.com/java-calendar)
- [Tutorial de Java LocalDate class](https://www.baeldung.com/java-8-date-time-intro)