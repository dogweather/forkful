---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "C++: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

La conversión de una fecha a una cadena en Arduino es simplemente cambiar un tipo de dato (estructura de fecha) en otro tipo más manejable (cadena). Los programadores hacen esto para simplificar y facilitar la visualización y el procesamiento de fechas.

## Cómo hacer:

Aquí tienes un código sencillo que convierte una fecha en una cadena en Arduino.

```Arduino
#include <RTClib.h> 

RTC_DS1307 rtc;

void setup () {
  rtc.begin();
  DateTime now = rtc.now(); 

  char fecha[16];
  sprintf(fecha, "%02d/%02d/%02d %02d:%02d:%02d", now.day(), now.month(), now.year(), now.hour(), now.minute(), now.second());

  Serial.begin(9600);
  Serial.println(fecha);
}
```

Lo que imprimimos sería algo así:

```
15/12/2021 12:30:00
```

## Inmersión profunda:

Históricamente, en Arduino las fechas se han convertido a cadenas manualmente debido a la limitada memoria disponible. Sin embargo, con la evolución de las librerías y los dispositivos, ahora se puede hacer automáticamente utilizando funciones como `sprintf()`.

Aunque `sprintf()` es la forma más común, también puede utilizar alternativas como `itoa()` para números enteros o `dtostrf()` para flotantes, aunque requerirán un proceso de concatenación de la fecha.

El proceso de conversión básicamente toma los componentes individuales de la fecha (día, mes, año, hora, minuto y segundo) y los convierte a cadenas, luego los une en una única cadena.

## Ver también:

- [La documentación oficial de Arduino sobre sprintf()](https://www.arduino.cc/reference/en/language/functions/characters/printf/).
- [El tutorial de Adafruit sobre RTC lib](https://learn.adafruit.com/ds1307-real-time-clock-breakout-board-kit).
- [Discusión en StackOverflow sobre la conversión de fechas en Arduino](https://stackoverflow.com/questions/8492968/display-the-date-and-time-in-lcd-by-using-arduino).