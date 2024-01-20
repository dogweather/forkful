---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Obtener la fecha actual en Arduino es la acción de adquirir la fecha y hora exacta donde se encuentra el programa ejecutándose. Es esencial para el registro de datos, control horario y marcar eventos.

## Cómo hacerlo:
Usaremos el módulo RTC (Real Time Clock) DS3231 que es muy preciso. Mira el ejemplo de código.

```Arduino
#include <Wire.h> 
#include "RTClib.h" 

RTC_DS3231 rtc; 

void setup () 
{ 
   while (!Serial); 
   if (! rtc.begin()) 
   { 
     Serial.println("No se encuentra el RTC"); 
     while (1); 
   }
   
   if (rtc.lostPower()) 
   { 
     rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
   } 
}

void loop () 
{ 
   DateTime now = rtc.now(); 
   Serial.print(now.year(), DEC);
   Serial.print('/');
   Serial.print(now.month(), DEC);
   Serial.print('/');
   Serial.println(now.day(), DEC);
   delay(3000);
} 
```
Al ejecutarse, esta porción de código producirá una salida que muestre la fecha actual en el formato AAAA / MM / DD.

## Profundización:
Arduino no tiene una función incorporada para obtener la fecha y hora actual. En sus inicios, la idea era construir una plataforma de microcontrolador simple y de bajo costo que no incluía un reloj en tiempo real. Ahora hay varias formas de obtener la fecha/hora actual, como usar un módulo RTC externo (como el DS3231 en nuestro ejemplo), o conectándose a Internet y obteniendo los datos de un servidor de hora en red.

## Ver también:
1. Biblioteca RTClib: https://github.com/adafruit/RTClib 
2. Módulo DS3231: http://www.alldatasheet.es/datasheet-pdf/pdf/502461/DALLAS/DS3231.html
3. Más detalles sobre Arduino: https://www.arduino.cc/