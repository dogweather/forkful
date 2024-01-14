---
title:                "Arduino: Calculando una fecha en el futuro o en el pasado"
simple_title:         "Calculando una fecha en el futuro o en el pasado"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

La programación en Arduino nos permite crear proyectos increíbles que pueden mejorar nuestras vidas de muchas maneras. Una de esas aplicaciones es el cálculo de una fecha en el futuro o pasado, lo cual puede ser útil para programar eventos o tareas.

## Cómo hacerlo

Para calcular una fecha en el futuro o pasado, primero necesitamos definir una fecha de referencia. En este caso, utilizaremos la función `millis()` de Arduino para obtener el tiempo actual en milisegundos.

```
Arduino

unsigned long tiempo_actual = millis();
```

Luego, podemos calcular una fecha en el futuro sumando una cantidad de milisegundos a la fecha de referencia. Por ejemplo, si queremos calcular la fecha exacta dentro de una semana, podemos sumar 604800000 milisegundos (7 días en milisegundos) al tiempo actual.

```
Arduino
unsigned long fecha_futura = tiempo_actual + 604800000;
```

Para calcular una fecha en el pasado, podemos restar una cantidad de milisegundos a la fecha de referencia. Si queremos saber la fecha exacta hace un mes, restaremos 2592000000 milisegundos (30 días en milisegundos) al tiempo actual.

```
Arduino
unsigned long fecha_pasada = tiempo_actual - 2592000000;
```

Una vez que tenemos la fecha en milisegundos, podemos convertirla a un formato legible para el usuario utilizando la función `millisToDateTime()` de la biblioteca ArduinoDateTime.

```
Arduino
#include <ArduinoDateTime.h> // incluimos la biblioteca

unsigned long fecha_futura = tiempo_actual + 604800000; // calculamos la fecha en el futuro
DateTime fecha_legible = millisToDateTime(fecha_futura); // convertimos la fecha en milisegundos a un formato legible
```

Por último, podemos imprimir la fecha en el puerto serie para que podamos verla en el monitor serial.

```
Arduino
// imprimimos la fecha en el puerto serie
Serial.println("La fecha en el futuro es " + String(fecha_legible.month()) + "/" + String(fecha_legible.day()) + "/" + String(fecha_legible.year()));
```

## Profundizando

Calcular una fecha en el futuro o pasado puede ser útil en aplicaciones de temporización, como encender y apagar luces automáticamente en un determinado día y hora. También puede ser útil en proyectos que involucren el registro de eventos en una fecha específica, como la recolección de datos meteorológicos.

Es importante tener en cuenta que este método de cálculo de fechas solo es preciso si el reloj interno de Arduino no se detiene o se reinicia. Si el Arduino se reinicia, el cálculo de fechas se reiniciará desde cero.

## Véase también

- [Biblioteca ArduinoDateTime](https://github.com/arduino-libraries/ArduinoDateTime)
- [Función millis() de Arduino](https://www.arduino.cc/reference/en/language/functions/time/millis/)
- [Tutorial sobre cómo calcular fechas en el Arduino Forum](https://forum.arduino.cc/index.php?topic=712493.0)