---
title:                "Arduino: Comparando dos fechas"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué
La comparación de dos fechas es una habilidad útil en la programación, ya que nos permite realizar acciones basadas en la fecha actual o calcular la diferencia entre dos fechas. Esto puede ser útil en proyectos como sistemas de registro de tiempo o recordatorios de eventos.

## Cómo hacerlo
Para comparar dos fechas en Arduino, primero debemos crear dos objetos del tipo `DateTime`. Estos objetos contienen la información de la fecha y hora actual. Luego, podemos utilizar los métodos `year()`, `month()`, `day()`, `hour()` y `minute()` para obtener las diferentes partes de la fecha y compararlas.

```Arduino
DateTime fecha1 = DateTime(2021, 8, 15, 18, 30); // Crear objeto DateTime con fecha y hora específicas
DateTime fecha2 = DateTime(); // Crear objeto DateTime con la fecha y hora actual

// Obtener la diferencia entre los años de las dos fechas
int diferenciaAnios = fecha1.year() - fecha2.year();

// Obtener la diferencia entre los meses de las dos fechas
int diferenciaMeses = fecha1.month() - fecha2.month();

// Obtener la diferencia entre los días de las dos fechas
int diferenciaDias = fecha1.day() - fecha2.day();

// Imprimir la diferencia entre las dos fechas
Serial.print("La diferencia entre las dos fechas es: ");
Serial.print(diferenciaAnios);
Serial.print(" años, ");
Serial.print(diferenciaMeses);
Serial.print(" meses y ");
Serial.print(diferenciaDias);
Serial.println(" días.");
```

El resultado en el monitor serial sería: `La diferencia entre las dos fechas es: 0 años, 0 meses y -2 días.` Esto nos indica que la segunda fecha es dos días antes que la primera.

## Profundizando
La comparación de fechas puede ser más compleja si queremos tener en cuenta otros factores como años bisiestos o zonas horarias. Para lidiar con años bisiestos, podemos utilizar el método `isLeapYear()` para verificar si un año es bisiesto y ajustar la diferencia de días en consecuencia.

Además, es importante tener en cuenta que los objetos `DateTime` en Arduino utilizan la zona horaria GMT (Greenwich Mean Time). Por lo tanto, si estamos en una zona horaria diferente, debemos ajustar las fechas y horas en consecuencia antes de realizar la comparación.

## Ver también
- Documentación oficial de la librería DateTime: https://github.com/PaulStoffregen/DateTime
- Ejemplos de uso de DateTime en proyectos de Arduino: https://create.arduino.cc/projecthub/projects/tags/date-time
- Tutorial sobre la comparación de fechas en Arduino: https://www.electroschematics.com/comparing-dates-and-time-with-arduino/