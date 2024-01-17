---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Arduino: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

¡Hola amigos programadores!

En este artículo, vamos a hablar sobre cómo calcular una fecha en el futuro o en el pasado utilizando Arduino. Pero primero, ¿qué significa exactamente calcular una fecha? Básicamente, se trata de determinar la fecha y hora de un evento futuro o pasado en relación a la fecha y hora actual.

¿Por qué los programadores necesitan saber cómo hacer esto? Bueno, puede ser útil en muchos proyectos donde se necesite programar eventos específicos en el tiempo, como por ejemplo, una alarma que se active en un día y hora determinados, o una aplicación que registre la fecha de un evento importante.

## ¿Cómo se hace?

Para calcular una fecha en el futuro o en el pasado en Arduino, utilizaremos la función ```millis()```. Esta función devuelve el número de milisegundos que han transcurrido desde que Arduino se encendió por última vez. A partir de este valor, podemos calcular fechas futuras o pasadas sumando o restando la cantidad de milisegundos correspondientes.

Por ejemplo, si queremos obtener la fecha y hora de 3 días en el futuro, podemos usar la función ```millis()``` para obtener el valor actual y luego sumarle 259200000 milisegundos (3 días en milisegundos). Es importante recordar que este valor es en milisegundos, por lo que si queremos obtener una fecha en minutos o segundos, deberemos convertirlo previamente.

Veamos un ejemplo de código práctico:

```Arduino
unsigned long tiempoActual = millis(); //obtenemos el valor actual en milisegundos
unsigned long fechaFutura = tiempoActual + (3 * 24 * 60 * 60 * 1000); //sumamos 3 días en milisegundos
```

Una vez que tengamos la fecha en milisegundos, podemos convertirla a un formato más legible utilizando la función ```nextInt()```, que nos devuelve un número específico de minutos, segundos o días en función del valor que le pasemos como parámetro.

## Profundizando en el tema

Este concepto de calcular fechas no es exclusivo de Arduino, sino que también se puede hacer en otros lenguajes de programación utilizando diferentes funciones y métodos.

Además, existen bibliotecas específicas para manejar fechas y horas en Arduino, como la conocida "DateTime", que facilita mucho el cálculo y formato de fechas en proyectos más complejos.

Si queremos calcular una fecha en el pasado o en el futuro basándonos en una fecha específica, también podemos utilizar las funciones ```day()```, ```month()```, ```year()```, ```hour()```, ```minute()``` y ```second()``` para obtener fácilmente el valor de cada componente de la fecha y luego hacer cálculos con ellos.

## Consulta también

Si quieres profundizar aún más en este tema, aquí te dejamos algunos enlaces relacionados:

- [Referencia de la función millis() en Arduino](https://www.arduino.cc/reference/en/language/functions/time/millis/)
- [Biblioteca DateTime para Arduino](https://playground.arduino.cc/Code/DateTime/)
- [Artículo sobre cómo calcular fechas en Java](https://www.baeldung.com/java-date-time-manipulation)
- [Página de documentación sobre manejo de fechas en C++](https://www.cplusplus.com/reference/ctime/)

¡Esperamos que este artículo te haya sido útil y que puedas utilizar esta función en tus próximos proyectos con Arduino! ¡Hasta la próxima!