---
title:    "Arduino: Generación de números aleatorios"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué
¿Te has preguntado alguna vez cómo crear un programa que genere números aleatorios? ¡En este artículo te enseñaré cómo hacerlo de manera sencilla utilizando Arduino!

## Cómo hacerlo
¡Lo primero que necesitas es tener un Arduino y el software de Arduino instalado en tu computadora! Luego, sigue estos pasos:

1. Abre el software de Arduino y crea un nuevo programa.
2. Declara una variable para almacenar el número aleatorio, por ejemplo "numAleatorio".
3. Usa la función "random(min, max)" para generar un número aleatorio entre "min" y "max". Por ejemplo, ```Arduino
numAleatorio = random(1, 10);```
4. Ahora puedes imprimir el número aleatorio generado usando la función "Serial.print()" como en el siguiente código: ```Arduino
Serial.print("El número aleatorio es: ");
Serial.println(numAleatorio);```
5. ¡Carga el programa a tu Arduino y comprueba la salida en el monitor serial!

## Profundizando
¿Cómo funciona exactamente la función "random(min, max)"? Esta función utiliza el generador de números pseudoaleatorios provisto por la librería "stdlib.h". Esto significa que los números generados no son realmente aleatorios, pero pueden parecerlo para ciertas aplicaciones. Si quieres conocer más sobre el algoritmo utilizado, puedes investigar sobre "Mersenne Twister".

## Ver también
Si quieres seguir explorando el mundo de la generación de números aleatorios con Arduino, aquí tienes algunos enlaces útiles:

- [Documentación oficial de Arduino sobre la función "random()"](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Generación de números aleatorios con Arduino y la librería "random"](https://www.arduinolibraries.info/libraries/random)
- [Tutorial de Arduino sobre cómo generar números aleatorios](https://www.arduino.cc/en/Tutorial/RandomNumbers)
- [Más información sobre el algoritmo "Mersenne Twister"](http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html)