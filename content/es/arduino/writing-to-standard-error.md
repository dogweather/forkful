---
title:                "Arduino: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir a la salida de error estándar en Arduino?

Escribir a la salida de error estándar puede ser útil al momento de depurar y encontrar errores en nuestro código de Arduino. Al mostrar los errores en tiempo real, podemos identificar y corregir problemas de manera más eficiente.

## Cómo escribir a la salida de error estándar

Para escribir a la salida de error estándar en Arduino, primero debemos incluir la biblioteca "Serial.h". Luego, podemos utilizar la función "Serial.println()" para imprimir nuestro mensaje en la salida de error estándar. Por ejemplo:

```Arduino
#include <Serial.h>

void setup() {
  // Iniciamos la comunicación serial a 9600 baudios
  Serial.begin(9600);
}

void loop() {
  // Mostramos un mensaje de error en la salida estándar
  Serial.println("¡Se ha producido un error!");
}
```
La consola de Arduino nos mostrará el mensaje "¡Se ha producido un error!" cada vez que se ejecute el bucle "loop()".

## Profundizando en la salida de error estándar

La salida de error estándar en Arduino utiliza la comunicación serial para enviar mensajes a la consola. Esto nos permite imprimir mensajes de error en tiempo real mientras nuestro código se está ejecutando. Además, podemos utilizar diferentes funciones de la biblioteca "Serial.h" para formatear nuestros mensajes y mostrar información adicional, como el valor de variables o la hora en la que ocurrió el error.

## Ver también

- [Documentación de Serial en Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Tutorial: Depuración con la salida de error estándar en Arduino](https://www.instructables.com/id/Arduino-Debugging-with-the-Standard-Error-Output-C/)