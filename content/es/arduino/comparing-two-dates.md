---
title:                "Comparando dos fechas"
html_title:           "C#: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Comparar dos fechas significa verificar cuál es anterior, cuál es posterior o si son iguales. Los programadores realizan estas comparaciones para realizar tareas como ordenar eventos, calcular la duración de un periodo o establecer recordatorios.

## Cómo Hacerlo:

Aquí veremos cómo comparar dos fechas usando la biblioteca TimeLib en Arduino. Primero, instala dicha biblioteca en tu entorno de desarrollo de Arduino. A continuación, vamos a comparar dos fechas.

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  
  // Crear dos tiempos
  time_t t1 = tmConvert_t(2021, 10, 20, 00, 00, 00); // Año, Mes, Día, Hora, Minutos y Segundos
  time_t t2 = tmConvert_t(2021, 11, 21, 00, 00, 00); 

  if(t1 < t2){
    Serial.println("t1 es antes que t2");
  }
  else if(t1 > t2){
    Serial.println("t1 es después que t2");
  }
  else{
    Serial.println("t1 y t2 son iguales");
  }
}

time_t tmConvert_t(int YYYY, byte MM, byte DD, byte hh, byte mm, byte ss)
{
   tmElements_t tm;
  
   // Año, mes, día, hora, minuto, segundo
   tm.Year = CalendarYrToTm(YYYY);
   tm.Month = MM;
   tm.Day = DD;
   tm.Hour = hh;
   tm.Minute = mm;
   tm.Second = ss;

   return makeTime(tm);
}

void loop() {
  // Nada aquí
}
```
El código genera el output:

```Arduino
t1 es antes que t2
```

## En Profundidad

Históricamente, el concepto de comparar dos fechas se remonta a tiempos en que los registros de eventos se trataban manualmente. Con la llegada de las computadoras, esta tarea se ha simplificado y se ha hecho más precisa.

Existe más de una manera de comparar fechas. Además de la biblioteca TimeLib, otra biblioteca popular es RTClib. También puedes hacerlo de forma manual, pero es más engorroso y con un margen de error mayor.

En cuanto a la implementación en el ejemplo anterior, utilizamos la función `tmConvert_t` para convertir los valores del calendario en un valor `time_t`, que es la cantidad de segundos transcurridos desde el 1 de enero de 1970. Luego, comparamos las fechas con operadores de comparación estándar.

## Ver También

- [Biblioteca TimeLib](https://github.com/PaulStoffregen/Time)
- [Biblioteca RTClib](https://github.com/adafruit/RTClib)