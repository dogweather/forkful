---
title:                "Escribiendo a la salida de error estándar"
html_title:           "Arduino: Escribiendo a la salida de error estándar"
simple_title:         "Escribiendo a la salida de error estándar"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir en el error estándar es una técnica que usan los programadores para enviar mensajes de error o advertencia al usuario. Esta práctica es útil para ayudar a los desarrolladores a identificar y solucionar problemas en sus programas.

## Cómo:

En Arduino, escribir en el error estándar se realiza utilizando la función "Serial.println()", la cual permite imprimir mensajes en el monitor serial. A continuación se muestra un ejemplo de código y su resultado:

```
#include <Arduino.h>

void setup() {
  Serial.begin(9600); // Iniciar comunicación serial
}

void loop() {
  int x = 5;
  // Imprimir mensaje de advertencia si el valor de x es menor a 10
  if (x < 10) {
    Serial.println("¡Atención! El valor de x es menor a 10.");
  }
}
```

```
¡Atención! El valor de x es menor a 10.
```

## Profundizando:

Escribir en el error estándar se originó en la programación en C, donde se utilizaba la función "fprintf(stderr, ...)" para escribir mensajes en el flujo de error estándar. Alternativamente, los programadores también pueden utilizar la función "Serial.print()" para imprimir mensajes en el monitor serial, sin agregar un salto de línea al final del texto.

## Ver también:

- [Serial.println() en la documentación de Arduino](https://www.arduino.cc/reference/es/language/functions/communication/serial/println/)
- [Diferencias entre Serial.println() y Serial.print()](https://arduino.stackexchange.com/questions/1602/whats-the-difference-between-serial-print-and-serial-println)