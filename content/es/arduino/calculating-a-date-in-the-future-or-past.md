---
title:                "Cálculo de una fecha en el futuro o pasado"
html_title:           "Arduino: Cálculo de una fecha en el futuro o pasado"
simple_title:         "Cálculo de una fecha en el futuro o pasado"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué? 

Calcular una fecha futura o pasada es determinar una fecha específica, respectivamente antes o después de una dada. Los programadores lo hacen para programar eventos, realizar análisis de datos y definir intervalos de tiempo.

## ¿Cómo hacerlo?

Aquí aprenderemos cómo calcular una fecha futura en Arduino usando la biblioteca `TimeLib`. El método `makeTime` nos permite transformar los elementos de una fecha y hora (año, mes, día, hora, minuto, segundo) en un valor de tiempo. Y el método `breakTime` descompone ese valor de tiempo en sus componentes. Nobleza obliga, Arduino no admite fechas anteriores a 1970.

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(11, 30, 00, 9, 7, 2023); // configura el tiempo actual: 11:30:00 09/07/2023
}

void loop() {
  time_t t = now(); // obtiene el tiempo actual
  tmElements_t tm;
  breakTime(t, tm); // descompone el tiempo en sus elementos
  tm.Year = tmYearToCalendar(tm.Year); // convierte el año a formato calendario

  Serial.print("Fecha actual: ");
  printDateTime(tm);
  
  tm.Day += 7; // calcula una fecha futura, 7 días después
  t = makeTime(tm); // convierte los elementos de la fecha y hora a un valor temporal
  breakTime(t, tm); // descompone el valor de tiempo en sus elementos
  tm.Year = tmYearToCalendar(tm.Year);

  Serial.print("Fecha futura: ");
  printDateTime(tm);
  
  delay(2000);
}

void printDateTime(tmElements_t tm){
  Serial.print(tm.Day);
  Serial.print("/");
  Serial.print(tm.Month);
  Serial.print("/");
  Serial.print(tm.Year+1970);
  Serial.print(" ");
  Serial.print(tm.Hour);
  Serial.print(":");
  Serial.print(tm.Minute);
  Serial.println(); 
}
```

Output:

```Arduino
Fecha actual: 9/7/2023 11:30
Fecha futura: 16/7/2023 11:30
```

## Profundizando 

Históricamente, los sistemas informáticos han representado fechas como la cantidad clásica de segundos transcurridos desde un tiempo y fecha específicos, usualmente 00:00:00 UTC el 1 de enero de 1970, un punto en el tiempo conocido como epoch Unix. Eso sí, Arduino no contiene un RTC (Real Time Clock), por lo que no mantiene el tiempo cuando se desconecta de la alimentación.

En cuanto a alternativas, puedes usar la biblioteca Arduino `RTClib`, que permite una fácil comunicación con los módulos RTC. Sin embargo, el tratamiento de fechas con esa biblioteca todavía se basa en el epoch Unix.

Respecto a los detalles de la implementación de calcular una fecha futura o pasada, debes tener cuidado con los años bisiestos y los límites mensuales. Por ejemplo, si añades 1 al valor del día 31 podría resultar en un valor no válido para algunos meses.

## Ver también

- [Documentación oficial de la biblioteca TimeLib](https://www.pjrc.com/teensy/td_libs_Time.html)
- [Algoritmo de Zeller para el cálculo del día de la semana](https://en.wikipedia.org/wiki/Zeller%27s_congruence)
- [Biblioteca RTClib de Adafruit para Arduino](https://github.com/adafruit/RTClib)