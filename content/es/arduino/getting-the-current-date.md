---
title:                "Obteniendo la fecha actual"
date:                  2024-01-20T15:13:02.391780-07:00
html_title:           "Bash: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"

category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
En programación, obtener la fecha actual significa acceder al día, mes y año en tiempo real. Los programadores lo hacen para registrar eventos, controlar procesos temporizados o mostrar información relevante en sus proyectos.

## Cómo hacerlo:
Para obtener la fecha en Arduino, necesitarás un módulo de reloj de tiempo real (RTC) como el DS3231. Primero, conecta el módulo a tu Arduino, luego usa este código:

```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("No se encuentra el RTC");
    while (1);
  }
}

void loop() {
  DateTime now = rtc.now();
  
  Serial.print("Fecha: ");
  Serial.print(now.day());
  Serial.print('/');
  Serial.print(now.month());
  Serial.print('/');
  Serial.println(now.year());
  delay(1000);
}
```
Salida de ejemplo en el monitor serie:
```
Fecha: 28/3/2023
```

## Inmersión Profunda
Históricamente, los Arduinos carecen de un reloj en tiempo real incorporado, por eso se usa un módulo externo como el DS3231 que mantiene la fecha y la hora con precisión. Existen alternativas como usar un módulo GPS o sincronizar con un servidor de tiempo en internet. Implementar la fecha y hora en hardware externo libera recursos del microcontrolador y proporciona precisión incluso con reinicios o pérdidas de energía.

## Ver También
- Documentación de RTClib: https://github.com/adafruit/RTClib
- Instructivo sobre cómo usar un módulo RTC con Arduino: https://www.arduino.cc/en/Tutorial/LibraryExamples/DS1302RTC
- Información sobre cómo sincronizar hora con NTP: https://lastminuteengineers.com/esp32-ntp-server-date-time-tutorial/
