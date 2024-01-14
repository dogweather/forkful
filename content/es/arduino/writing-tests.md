---
title:                "Arduino: Escribir pruebas"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir pruebas (tests) en Arduino?

Escribir pruebas (tests) en Arduino es una forma efectiva de garantizar que el código que estamos escribiendo funciona correctamente. Las pruebas nos ayudan a detectar errores y problemas en nuestro código antes de implementarlo en un dispositivo físico. También nos permiten realizar cambios en nuestro código de manera segura, sin preocuparnos de romper funcionalidades previamente implementadas.

## Cómo escribir pruebas en Arduino

Para escribir pruebas en Arduino, utilizamos la biblioteca "ArduinoUnit". Esta biblioteca nos permite definir casos de prueba (test cases) y verificar los resultados esperados. Aquí hay un ejemplo sencillo de cómo escribir una prueba para una función que suma dos números:

```Arduino
#include <ArduinoUnit.h>

test(sumaTest) {
  int a = 5;
  int b = 10;
  assertEquals(a + b, 15);
}

unittest_main();
```

Este código define una función de prueba llamada "sumaTest". Dentro de esta función, definimos dos variables y utilizamos el método "assertEquals" para verificar que la suma de ambas variables sea igual a 15. Si esto es cierto, la prueba se considera exitosa.

## Profundizando en la escritura de pruebas en Arduino

Para escribir pruebas efectivas en Arduino, es importante seguir algunas buenas prácticas. Una de ellas es asegurarse de que cada prueba sea independiente y no dependa de otras pruebas para su ejecución. Además, es recomendable nombrar las pruebas de manera significativa para saber exactamente qué se está probando.

Otra buena práctica es utilizar el método "setup" para preparar nuestras pruebas, por ejemplo, estableciendo los valores iniciales de las variables que utilizaremos. De la misma forma, es importante utilizar el método "tearDown" para limpiar cualquier cambio que hayamos hecho en nuestro entorno de prueba.

También es recomendable utilizar "asserts" específicos para cada tipo de dato en lugar de utilizar siempre "assertEquals". Por ejemplo, para verificar que dos variables de tipo "float" sean iguales, podemos utilizar el método "assertEqualFloats" en lugar de "assertEquals".

## Ver también

- Biblioteca ArduinoUnit: https://github.com/mmurdoch/arduinounit
- Tutorial de ArduinoUnit: https://learn.adafruit.com/arduino-unit-testing?view=all
- Consejos para escribir pruebas en Arduino: https://rosagulla.es/arduino-testing-tips/