---
title:                "Arduino: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/writing-tests.md"
---

{{< edit_this_page >}}

¿Por qué escribir pruebas en Arduino?

Es importante escribir pruebas en Arduino para asegurarse de que el código funciona correctamente. Las pruebas pueden ayudar a detectar errores y a garantizar que el Arduino funcione como se espera.

## Cómo hacerlo

Hay varias formas de escribir pruebas en Arduino, pero aquí vamos a enfocarnos en el uso de la biblioteca "ArduinoUnit". Primero, debemos descargar e instalar la biblioteca en el IDE de Arduino. Una vez instalada, podemos comenzar a escribir nuestras pruebas.

Primero, debemos incluir la biblioteca en nuestro código con la siguiente línea:

```Arduino
#include <ArduinoUnit.h>
```

Luego, podemos comenzar a escribir nuestras pruebas utilizando las macros proporcionadas por la biblioteca. Por ejemplo, si queremos probar si una variable tiene el valor esperado, podemos usar la macro "assertEqual". Aquí hay un ejemplo de una prueba simple que verifica si una variable es igual a 5:

```Arduino
bool test_equal(){
  int x = 5;
  assertEqual(x, 5);
}
```

La biblioteca también proporciona macros para probar otros tipos de condiciones, como "assertTrue" y "assertFalse" para verificar si una condición es verdadera o falsa, respectivamente.

Una vez que hayamos escrito nuestras pruebas, debemos compilar y cargar el código en el Arduino. Si todas las pruebas pasan, significa que el código está funcionando como se espera. Pero si una prueba falla, significa que ha habido un error en el código y es necesario realizar una depuración.

## Un vistazo más profundo

Las pruebas en Arduino nos permiten hacer verificaciones muy específicas en nuestro código y nos ayudan a encontrar posibles errores más rápidamente. Además, con las pruebas podemos asegurarnos de que nuestro código sigue funcionando correctamente después de realizar cambios en él.

Es importante tener en cuenta que las pruebas no garantizan que el Arduino funcionará correctamente en todas las situaciones, pero sí nos brindan más confianza y seguridad en nuestro código.

## Ver también

- [Guía de la biblioteca ArduinoUnit](https://github.com/mmurdoch/arduinounit)
- [Documentación oficial de Arduino](https://www.arduino.cc/en/Tutorial/ArduinoUnit)
- [Ejemplos de pruebas en Arduino](https://github.com/mmurdoch/arduinounit/tree/master/examples)