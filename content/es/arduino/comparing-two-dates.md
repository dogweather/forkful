---
title:    "Arduino: Comparando dos fechas"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Por qué comparar dos fechas en programas Arduino

Comparar dos fechas es una práctica común en la programación de Arduino, ya que permite realizar diversas tareas relacionadas con el tiempo y la planificación. Por ejemplo, puede ser útil para activar un sensor en una fecha determinada, crear un temporizador o incluso para llevar un registro de eventos en un calendario.

## Cómo hacerlo

Para comparar dos fechas en Arduino, primero debes asegurarte de que tus fechas estén en un formato legible para el programa. Puedes usar la clase ```DateTime``` del reloj en tiempo real (RTC) para definir fechas con variables.

```
#include <RTClib.h>
RTC_DS3231 rtc;

DateTime fecha1;
DateTime fecha2;
```

Una vez que tienes tus fechas definidas, puedes usar la función ```difDays()``` de la biblioteca ```RTClib``` para obtener la diferencia, en días, entre dos fechas.

```
int diferencia = fecha1.difDays(fecha2);
```

Luego puedes utilizar la variable ```diferencia``` en condiciones, como un ```if```, para realizar acciones basadas en la comparación entre las dos fechas.

## Profundizando

Si quieres profundizar en la comparación de fechas en Arduino, es importante tener en cuenta que las fechas deben ser definidas en el formato adecuado. Además, ten en cuenta que la biblioteca ```RTClib``` utilizada en este ejemplo solo es válida para fechas entre el año 2000 y el 2099.

Existen otras bibliotecas y métodos para comparar fechas en Arduino, como utilizar la función ```time.h``` y el tiempo en milisegundos, o la biblioteca ```Time``` y la función ```elapsedDays()```. Investigar y experimentar con diferentes métodos puede ser beneficioso para encontrar el que mejor se adapte a tus necesidades.

# Ver también

- [Biblioteca RTClib para Arduino](https://github.com/adafruit/RTClib)
- [Documentación de la función difDays()](https://learn.adafruit.com/arduino-lesson-12-lcd-displays-part-2?view=all#difdays)
- [Guía de referencia para la biblioteca Time](https://www.arduino.cc/en/Reference/Time)
- [Tutorial de Time.h en Arduino](https://www.antriendep.com/2018/03/20/arduino-tutoiral-usar-timer-y-diferentes-funciones-de-la-libreria-time-h/)
- [Tutorial de la biblioteca Time en Arduino](https://randomnerdtutorials.com/arduino-basics-set-time-date-arduino/)