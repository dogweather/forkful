---
title:                "Arduino: Obtener la fecha actual."
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

##Por qué

¿Alguna vez has querido que tu proyecto de Arduino muestre la fecha y hora actual? Quizás quieres crear un reloj digital o simplemente quieres saber cuánto tiempo ha pasado desde tu último proyecto. Sea cual sea la razón, obtener la fecha y hora actual es una habilidad útil para cualquier programador de Arduino.

##Cómo hacerlo

Para obtener la fecha y hora actual en Arduino, debemos utilizar la librería "Time.h". Primero, debemos incluir la librería en nuestro código:

```Arduino
#include <Time.h>
```

Luego, debemos inicializar una variable del tipo "tmElements_t" que utilizará la librería para almacenar la fecha y hora:

```Arduino
tmElements_t tiempo;
```

Ahora, podemos utilizar la función "now()" para obtener la fecha y hora actual y almacenarla en nuestra variable "tiempo":

```Arduino
tiempo = now();
```

Para imprimir la fecha y hora en el monitor serie, utilizamos las funciones "year()", "month()", "day()", "hour()", "minute()" y "second()" de la librería, de la siguiente manera:

```Arduino
Serial.print(year(tiempo)); // imprime el año actual
Serial.print("/");
Serial.print(month(tiempo)); // imprime el mes actual
Serial.print("/");
Serial.print(day(tiempo)); // imprime el día actual

Serial.print(" ");

Serial.print(hour(tiempo)); // imprime la hora actual
Serial.print(":");
Serial.print(minute(tiempo)); // imprime el minuto actual
Serial.print(":");
Serial.print(second(tiempo)); // imprime el segundo actual
```

La salida en el monitor serie debería verse así:

```
2021/09/03 12:30:25
```

##Profundizando

La función "now()" de la librería "Time.h" utiliza un reloj interno del Arduino y se actualiza cada segundo. Sin embargo, si necesitas una mayor precisión, puedes utilizar un módulo de tiempo real (RTC) externo que se conecta al Arduino a través de I2C.

Además, la librería "Time.h" también tiene funciones para obtener la fecha y hora en formato Unix, o para convertir entre diferentes formatos de tiempo.

##Véase también

- [Documentación de Time.h](https://www.arduino.cc/reference/en/libraries/time/)
- [Tutorial sobre el uso de un módulo RTC con Arduino](https://www.youtube.com/watch?v=92AroynoCXc)
- [Ejemplo de conversión de formato de tiempo en Arduino](https://lastminuteengineers.com/convert-time-date-to-timestamp-arduino-ide/)