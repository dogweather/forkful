---
title:    "Arduino: Calculando una fecha en el futuro o pasado"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## ¿Por qué calcular una fecha en el futuro o pasado?

Calcular una fecha en el futuro o pasado puede ser útil en muchos proyectos de Arduino, como sistemas de control de tiempo o alarmas. También puede ser una habilidad valiosa para aprender en la programación en general.

## Cómo hacerlo:

Para calcular una fecha en el futuro o pasado en Arduino, puedes seguir los siguientes pasos:

1. Definir la fecha de inicio: primero, debes establecer la fecha de inicio a partir de la cual comenzarás a calcular. Esto se puede hacer utilizando la función ```setTime ()``` de la librería ```Time```. Por ejemplo: ```setTime (12, 0, 0, 1, 1, 2021)``` establece la fecha y hora en enero 1, 2021 a las 12:00:00 PM.

2. Establecer una variable para almacenar la fecha del futuro o pasado: puedes usar la estructura ```tmElements_t``` de la librería ```Time``` para crear una variable que almacene la fecha en el futuro o pasado que deseas calcular.

3. Calcula la diferencia de tiempo: para calcular la fecha en el futuro o pasado, debes obtener la diferencia de tiempo entre la fecha de inicio y la fecha deseada. En Arduino, la función ```makeTime ()``` de la librería ```Time``` puede ayudarte a hacer esto. Por ejemplo: ```makeTime (0, 0, 0, 15, 2, 2021)``` creará una variable con la diferencia de tiempo de 15 días, 0 horas y 0 minutos, estableciéndola en febrero 15, 2021.

4. Sumar o restar la diferencia de tiempo a la fecha de inicio: finalmente, usa la función ```adjustTime ()``` de la librería ```Time``` para agregar o restar la diferencia de tiempo a la fecha de inicio establecida en el primer paso. Esto actualizará la variable que almacena la fecha en el futuro o pasado.

## Profundizando:

Calcular una fecha en el futuro o pasado requiere una comprensión de cómo se almacenan y representan las fechas y el tiempo en Arduino utilizando la librería ```Time```. Si quieres profundizar más en el tema, puedes investigar sobre la estructura ```tmElements_t``` y cómo se usa para almacenar fechas y tiempos en Arduino.

## Ver también:

- [Tutorial de códigos básicos de Arduino](https://www.arduino.cc/en/Tutorial/Code)
- [Guía oficial de la librería de tiempo de Arduino](https://www.pjrc.com/teensy/td_libs_Time.html)
- [Ejemplos prácticos de uso de la librería de tiempo de Arduino](https://lastminuteengineers.com/arduino-rtc-ds3231-tutorial/)