---
title:    "Arduino: Obteniendo la fecha actual"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

#¿Por qué utilizar Arduino para obtener la fecha actual?

Si está trabajando en un proyecto de Arduino que requiere saber la fecha actual, como un sistema de registro de datos o una alarma programada, es esencial saber cómo obtener la fecha actual utilizando Arduino. Afortunadamente, esto es bastante simple de hacer y puede ser muy útil en varios proyectos de Arduino.

## Cómo hacerlo

Para obtener la fecha actual utilizando Arduino, primero debemos incluir la biblioteca de tiempo en nuestro código. Esta biblioteca nos permitirá obtener información sobre la fecha y la hora en tiempo real.

```Arduino
#include <Time.h> // incluir la biblioteca de tiempo
```

A continuación, necesitamos establecer una variable del tipo tmElements_t que almacenará la fecha actual.

```Arduino
tmElements_t fecha; // variable para almacenar la fecha actual
```

Ahora, utilizando la función "breakTime", podemos obtener la fecha actual y almacenarla en nuestra variable "fecha".

```Arduino
breakTime(now(), fecha); // obtener la fecha actual y almacenarla en la variable "fecha"
```

Finalmente, podemos imprimir la fecha actual a través de las funciones "year", "month" y "day", que nos permiten acceder a cada componente de la fecha por separado.

```Arduino
Serial.print("Hoy es "); // imprimir texto
Serial.print(day(fecha)); // imprimir el día actual
Serial.print("/"); // imprimir el separador "/"
Serial.print(month(fecha)); // imprimir el mes actual
Serial.print("/"); // imprimir el separador "/"
Serial.println(year(fecha)); // imprimir el año actual
```

¡Eso es todo! Ahora podrá obtener la fecha actual usando Arduino en sus proyectos.

## Un poco más profundo

Si desea conocer más detalles sobre la función "breakTime" que utiliza para obtener la fecha actual, puede consultar la documentación de la biblioteca de tiempo de Arduino. Esta función toma un valor de tiempo en segundos desde una fecha determinada (1 de enero de 1970) y lo descompone en sus componentes (año, mes, día, etc.).

También puede utilizar la función "now" para obtener el valor de tiempo actual en segundos y luego utilizar la función "breakTime" para descomponerlo en sus componentes.

```Arduino
tmElements_t fecha; // variable para almacenar la fecha actual
unsigned long tiempoActual = now(); // obtener el tiempo actual en segundos
breakTime(tiempoActual, fecha); // descomponer el tiempo actual en sus componentes
```

## Ver también

- [Documentación de la biblioteca de tiempo de Arduino](https://www.arduino.cc/en/Reference/Time)
- [Tutorial de Adafruit sobre cómo obtener la fecha actual con Arduino](https://learn.adafruit.com/arduino-time-and-date/overview)