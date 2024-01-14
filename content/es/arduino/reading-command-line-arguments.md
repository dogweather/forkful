---
title:                "Arduino: Lectura de argumentos de línea de comandos"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

Si eres nuevo en la programación de Arduino, puede ser confuso saber cómo interactuar con tu dispositivo a través de la línea de comandos. Sin embargo, leer argumentos de línea de comandos puede ser muy útil en ciertas situaciones, como cuando necesitas pasar información a tu programa de forma dinámica. En este post, aprenderás cómo leer argumentos de línea de comando en tu proyecto de Arduino.

## Cómo hacerlo

Primero, debes entender que los argumentos de línea de comando son valores que se pasan al programa al iniciarlo. Esto permite introducir información adicional en tu programa sin necesidad de cambiar el código. Para leer estos argumentos, sigue los siguientes pasos:

1. Incluye la biblioteca "Arduino.h" en tu proyecto.
```
include "Arduino.h"
```

2. Declara las variables que quieres recibir como argumentos.
```
int arg1;
float arg2;
```

3. En la función setup(), utiliza la función "Serial.begin()" para habilitar la comunicación serial.
```
Serial.begin(9600);
```

4. En la función loop(), utiliza la función "Serial.available()" para verificar si hay argumentos disponibles y la función "Serial.parseFloat()" para leer números decimales y "Serial.parseInt()" para leer números enteros. Luego, puedes imprimir los valores leídos utilizando la función "Serial.print()".
```
void loop() {
  if (Serial.available()) {
    arg1 = Serial.parseFloat();
    arg2 = Serial.parseInt();
    Serial.print("Arg1: ");
    Serial.println(arg1);
    Serial.print("Arg2: ");
    Serial.println(arg2);
  }
}
```

5. Ejecuta el programa y verás que los argumentos son leídos y mostrados en el monitor serial.

## Profundizando

Existen otras funciones que puedes utilizar para leer argumentos de línea de comando, como "Serial.readString()" para leer cadenas de texto y "Serial.read()" para leer un solo carácter. Además, puedes pasar múltiples argumentos separándolos con un espacio en blanco.

Puedes combinar la lectura de argumentos de línea de comando con otras funcionalidades de Arduino, como sensores, para crear proyectos más complejos y dinámicos.

## Ver también

Para más información sobre cómo leer argumentos de línea de comando en Arduino, puedes consultar los siguientes recursos:

- [Documentación oficial de Arduino sobre comunicación serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Tutorial de Hackster sobre cómo leer argumentos en Arduino](https://www.hackster.io/rraghuvanshi456/how-to-pass-command-line-arguments-to-arduino-code-3e809b)