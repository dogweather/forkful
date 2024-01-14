---
title:                "Arduino: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has querido que tu programa Arduino pueda aceptar diferentes entradas de manera dinámica? ¡Entonces leer argumentos de línea de comando es algo que definitivamente querrás aprender! Esta técnica te permitirá controlar tu programa enviando diferentes comandos de una sola vez, ahorrándote tiempo y esfuerzo.

## Cómo hacerlo

El proceso de leer argumentos de línea de comando en Arduino es bastante sencillo. Todo lo que necesitas es seguir estos sencillos pasos:

1. Necesitarás utilizar la biblioteca "Serial". Si aún no la tienes instalada, puedes hacerlo fácilmente desde el "Administrador de bibliotecas" en el software de Arduino.

2. Una vez que tengas la biblioteca instalada, asegúrate de incluirla en tu programa con la siguiente línea de código:

```Arduino
#include <Serial.h>
```

3. Ahora, para leer los argumentos de línea de comando, utilizaremos la función `Serial.parseInt()`. Esta función lee el primer número entero que recibe a través del puerto serie de Arduino.

4. Si deseas obtener más de un argumento, puedes utilizar la función `Serial.readString()` que lee y almacena una cadena de caracteres enviada a través del puerto serie. Luego, puedes utilizar la función `toInt()` para convertir esa cadena en un número entero.

¡Y eso es todo! Con estos simples pasos, podrás leer argumentos de línea de comando en tu programa Arduino.

## Profundizando

Si deseas ir un paso más allá, también puedes utilizar la función `indexOf()` para buscar en la cadena recibida un carácter específico y obtener diferentes argumentos en una sola línea.

Por ejemplo, si envías la siguiente cadena a través del puerto serie: "25,50,75", utilizando la función `indexOf(",")` puedes obtener los números 25, 50 y 75 como argumentos individuales.

Esto puede ser especialmente útil si deseas enviar una serie de comandos a tu programa de Arduino a través de una sola cadena.

## Ver también

- [Documentación oficial de Serial.parseInt()](https://www.arduino.cc/reference/en/language/functions/communication/serial/parseint/)
- [Documentación oficial de Serial.readString()](https://www.arduino.cc/reference/en/language/functions/communication/serial/readstring/)
- [Documentación oficial de indexOf()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/indexof/)