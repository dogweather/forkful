---
title:                "Análisis de una fecha a partir de una cadena"
date:                  2024-01-20T15:34:28.261720-07:00
html_title:           "Arduino: Análisis de una fecha a partir de una cadena"
simple_title:         "Análisis de una fecha a partir de una cadena"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (¿Qué y Por Qué?)
Parsear una fecha desde un string implica convertir texto que representa una fecha ("25/12/2022") en un formato que el programa pueda entender y manipular. Los programadores hacen esto para facilitar operaciones como comparar fechas, calcular períodos de tiempo, o simplemente mostrar las fechas de manera legible y coherente.

## How to: (Cómo hacerlo:)
```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  // Verifica si la RTC está conectada correctamente
  if (!rtc.begin()) {
    Serial.println("No se encuentra RTC");
    while (1);
  }
  // Parsear una fecha desde un string y ajustar la hora de la RTC
  rtc.adjust(DateTime(__DATE__, __TIME__)); // setea la fecha y hora con la fecha y hora de compilación
  // Si quisieras ajustar a una fecha específica desde un string, podrías hacerlo así:
  // rtc.adjust(DateTime("2022/12/25 18:30:00"));
}

void loop() {
  DateTime now = rtc.now();
  
  // Imprimir la fecha en diferentes formatos
  Serial.print(now.day());
  Serial.print('/');
  Serial.print(now.month());
  Serial.print('/');
  Serial.print(now.year());
  Serial.print(" ");
  Serial.print(now.hour());
  Serial.print(':');
  Serial.print(now.minute());
  Serial.print(':');
  Serial.print(now.second());
  Serial.println();
  
  delay(1000);
}
```
Salida de ejemplo:
```
25/12/2022 18:30:00
```

## Deep Dive (Profundizando)
Parsear fechas en Arduino puede ser tan simple como usar la función `adjust` de la librería RTClib o tan complejo como escribir una función propia para manejar formatos de fecha específicos. Históricamente, el manejo de fechas ha sido un proceso propenso a errores debido a las variaciones de formateo entre culturas y las diferencias en la representación de zonas horarias. Alternativas como el uso de sellos de tiempo UNIX han simplificado la comparación y cálculo de diferencias entre fechas. La implementación de parsing es crucial en proyectos que involucran programación de eventos, almacenamiento de registros temporales o interfaces con usuario que requieren precision temporal.

## See Also (Véase También)
- [Biblioteca RTClib](https://github.com/adafruit/RTClib)
- [Manejo de fechas y horas en C++](http://www.cplusplus.com/reference/ctime/)
- [Documentación oficial de Arduino](https://www.arduino.cc/reference/en)
- [Foro de soporte de Arduino](https://forum.arduino.cc/)
