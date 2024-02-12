---
title:                "Convirtiendo una fecha en una cadena de texto"
aliases: - /es/arduino/converting-a-date-into-a-string.md
date:                  2024-01-20T17:36:13.881470-07:00
model:                 gpt-4-1106-preview
simple_title:         "Convirtiendo una fecha en una cadena de texto"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Convertir una fecha en una cadena de texto permite mostrarla o procesarla de manera legible para humanos. Los programadores lo hacen para interactuar con interfaces de usuario, registrar eventos o comparar fechas de forma sencilla.

## Cómo hacerlo:
Aquí te muestro cómo puedes convertir una fecha en cadena:

```Arduino
#include <Wire.h>  
#include <RTClib.h>  

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("No se encuentra RTC");
    while (1);
  }
}

void loop() {
  DateTime now = rtc.now();
  
  char fechaComoCadena[20];
  snprintf(fechaComoCadena, sizeof(fechaComoCadena), "%02d/%02d/%04d %02d:%02d:%02d", now.day(), now.month(), now.year(), now.hour(), now.minute(), now.second());

  Serial.println(fechaComoCadena);
  delay(1000);
}
```
Salida de muestra:
```
24/03/2023 16:50:03
```
## Conocimiento en Detalle:
Convertir fechas a cadenas se ha hecho desde que las computadoras empezaron a usar fechas para alguna función útil. En Arduino, usamos bibliotecas como `RTClib` para trabajar con módulos de reloj en tiempo real (RTC) que pueden mantener la hora actual incluso con el Arduino apagado. Otras alternativas para convertir fechas incluyen usar `sprintf` o manipular la fecha manualmente.

Detalles de implementación: `snprintf` es seguro en cuanto al tamaño del buffer, evitando desbordamientos. Las cadenas de formato dentro de `snprintf` definen cómo se convertirá la fecha y hora a texto (por ejemplo, `%02d` asegura que el número siempre tenga dos dígitos, rellenando con ceros si es necesario).

## Ver También:
- Documentación de Arduino para snprintf: https://www.arduino.cc/reference/en/language/functions/characters/snprintf/
- RTClib, una biblioteca para usar con RTC: https://github.com/adafruit/RTClib
- Información sobre el RTC DS3231, uno de los módulos de reloj más precisos para Arduino: https://www.maximintegrated.com/en/products/analog/real-time-clocks/DS3231.html
