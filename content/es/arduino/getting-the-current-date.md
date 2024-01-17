---
title:                "Obteniendo la fecha actual"
html_title:           "Arduino: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¡Qué y Por Qué?

Obtener la fecha actual en programación se refiere a obtener la información de la fecha y hora actuales. Los programadores suelen hacerlo para realizar acciones en función de la hora o fecha actual, como activar una alarma, registrar eventos o sincronizar dispositivos.

## ¡Cómo hacerlo!

```Arduino
#include <DateTime.h>

void setup() {
  Serial.begin(9600); // Iniciar la comunicación serial
  DateTime now = DateTime(F(__DATE__), F(__TIME__)); // Asignar valores de fecha y hora actuales a una variable
  int day, month, year, hour, minute;
  day = now.day(); // Obtener el día actual
  month = now.month(); // Obtener el mes actual
  year = now.year(); // Obtener el año actual
  hour = now.hour(); // Obtener la hora actual
  minute = now.minute(); // Obtener los minutos actuales
  Serial.print("Fecha actual: ");
  Serial.print(day); // Imprimir el día
  Serial.print("/"); // Imprimir una barra
  Serial.print(month); // Imprimir el mes
  Serial.print("/"); // Imprimir una barra
  Serial.println(year); // Imprimir el año
  Serial.print("Hora actual: ");
  Serial.print(hour); // Imprimir la hora
  Serial.print(":"); // Imprimir dos puntos
  Serial.println(minute); // Imprimir los minutos
}

void loop() {
  // código a ejecutar repetidamente
}
```

## Inmersión Profunda

En el pasado, obtener la fecha actual en programación era una tarea compleja y requería mucho código. Sin embargo, gracias a bibliotecas como DateTime.h, esto se ha vuelto mucho más sencillo y eficiente.

Una alternativa para obtener la fecha actual es utilizar un módulo de tiempo real (RTC) como el DS1307 o DS3231. Estos módulos tienen un reloj interno y se comunican con el Arduino a través de la interfaz I2C.

Para implementar correctamente el código para obtener la fecha y hora actuales, es importante asegurarse de que el reloj del Arduino esté configurado correctamente. Además, es útil utilizar variables para almacenar los valores de la fecha y hora actuales para un acceso más fácil y legible en el código.

## Véase También

- [DateTime.h - Documentación oficial](https://github.com/PaulStoffregen/DateTime)
- [Arduino DS1307 RTC](https://www.arduino.cc/en/Tutorial/BuiltInExamples/DS1307RTC)
- [DS3231 Real Time Clock](https://www.arduino.cc/en/Tutorial/RTC)

¡Ya está! ¡Ahora puedes obtener fácilmente la fecha y hora actuales en tus proyectos de Arduino usando la biblioteca DateTime.h! ¡Sigue explorando y descubriendo nuevas formas de utilizar esta funcionalidad en tus proyectos!