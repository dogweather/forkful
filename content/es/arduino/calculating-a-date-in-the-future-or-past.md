---
title:                "Arduino: Calculando una fecha en el futuro o pasado"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Por qué calcular fechas en el futuro o pasado?

El cálculo de fechas en el futuro o pasado es útil en proyectos de Arduino que requieren programar eventos en ciertos momentos específicos. Por ejemplo, puede ser útil para encender una luz en una fecha y hora determinadas o programar un sistema de riego para que se active en ciertos días.

## Cómo hacerlo

Para calcular fechas en el futuro o pasado en Arduino, se pueden seguir los siguientes pasos:

1. Definir una variable para almacenar la fecha actual.
```Arduino
DateTime fechaActual = now();
```
2. Definir una variable para almacenar la fecha en el futuro o pasado.
```Arduino
DateTime fechaCalculada = DateTime(año, mes, día, hora, minuto, segundo);
```
3. Usar la función `calculateOffset()` junto con las variables de fecha para obtener la diferencia en segundos.
```Arduino
int segundos = calculateOffset(fechaActual, fechaCalculada);
```
4. Utilizar la función `setTime()` para establecer la fecha en el reloj de Arduino.
```Arduino
setTime(segundos);
```
5. ¡Listo! Ahora el reloj de Arduino estará configurado en la fecha y hora deseada.

## Profundizando

El cálculo de fechas en el futuro o pasado en Arduino se basa en la función `calculateOffset()`, que toma en cuenta la fecha actual y la fecha deseada para obtener la diferencia en segundos. Esta diferencia se utiliza luego con la función `setTime()` para establecer la fecha en el reloj de Arduino.

Es importante tener en cuenta que la precisión de la fecha y hora en Arduino depende del tipo de reloj utilizado. Por lo general, se recomienda utilizar un reloj de tiempo real (RTC) para obtener una mayor precisión en las fechas y horas programadas.

## Ver también

- [Librería RTClib para Arduino](https://github.com/adafruit/RTClib)
- [Tutorial: Controlando un sistema de riego con Arduino](https://create.arduino.cc/projecthub/Arnov_Sharma/control-your-water-supply-solenoird-valve-using-arduino-9a4618)
- [Ejemplo de cálculo de fechas en Arduino](https://arduinohcs.files.wordpress.com/2012/07/calculating-time-in-arduino.pdf)