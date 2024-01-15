---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "Arduino: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

¡Hola a todos! ¿Alguna vez te has preguntado cómo convertir una fecha en una cadena de texto? En este artículo te mostraré cómo hacerlo utilizando Arduino. ¡Así que adelante y sumérgete!

## ¿Por qué?

A veces, cuando trabajamos con fechas en Arduino, es necesario convertirlas en una cadena de texto para mostrarlas en una pantalla o almacenarlas en una memoria. Con esta conversión, podemos darle formato a la fecha según nuestras necesidades y utilizarla de diferentes maneras en nuestro proyecto.

## Cómo hacerlo

Convertir una fecha en una cadena de texto es bastante sencillo y solo necesitas seguir unos pocos pasos. Primero, debes incluir la biblioteca "TimeLib.h" en tu código de Arduino. Esta biblioteca te permitirá manejar fechas y horas de una manera más fácil.

```Arduino
#include <TimeLib.h>
```

Luego, debes definir la estructura de fecha usando la función "setTime()". Esta función toma como parámetros el día, mes y año actual y los almacena en tres variables diferentes.

```Arduino
int day = 5;
int month = 10;
int year = 2021;
setTime(0, 0, 0, day, month, year);
```

Ahora, puedes usar la función "printDigits()" para dar formato a la fecha y almacenarla en una cadena de texto. Esta función toma dos parámetros, el valor numérico y la longitud de la cadena de texto que quieres obtener. Por ejemplo, si quieres mostrar el día con dos dígitos, puedes usar la función de esta manera:

```Arduino
String dayString = String(printDigits(day, 2));
```

Puedes repetir este proceso para el mes y el año y luego concatenar todas las cadenas de texto en una sola usando el operador de concatenación "+".

```Arduino
String dateString = dayString + "/" + monthString + "/" + yearString;
```

¡Y listo! Ahora tienes una cadena de texto con la fecha en el formato que deseas.

## Profundizando

Si quieres conocer más sobre la conversión de fechas en cadenas de texto, puedes explorar diferentes funciones de la biblioteca "TimeLib.h". Por ejemplo, puedes utilizar la función "monthShortStr()" para obtener el nombre corto del mes en vez del número. También puedes experimentar con diferentes formatos de fecha y encontrar el que mejor se adapte a tus necesidades.

¡Es hora de poner en práctica lo aprendido y darle formato a tus fechas en Arduino!

## Ver también

- [Documentación de la biblioteca TimeLib](https://www.arduino.cc/en/Reference/TimeManipulation)
- [Ejemplos de la biblioteca TimeLib](https://www.arduino.cc/en/Reference/TimeExamples)
- [Más información sobre fechas y horas en Arduino](https://arduinogetstarted.com/tutorials/arduino-date-time)