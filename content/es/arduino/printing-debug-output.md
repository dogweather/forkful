---
title:                "Imprimiendo salida de depuración"
html_title:           "Arduino: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Imagina esto: Estás escribiendo un código en Arduino y todo parece funcionar bien hasta que de repente, la luz LED que debería estar parpadeando no lo hace. ¿Pero qué pudo haber salido mal? Aquí es donde imprimir la salida de depuración o "debug output" en inglés, llega al rescate. 

La impresión de salida de depuración es simplemente mostrar mensajes en la consola de la computadora para saber si una parte de nuestro código se está ejecutando correctamente. Los programadores lo usan para encontrar errores en su código y solucionarlos de manera más eficiente.

## Cómo hacerlo:

Para imprimir la salida de depuración en un código de Arduino, necesitamos usar la función `Serial.print()` o `Serial.println()`, seguida de los datos que deseamos imprimir entre paréntesis. Por ejemplo:

```Arduino
int x = 5;
Serial.print("El valor de x es ");
Serial.println(x);
```

Esto imprimirá en la consola: `El valor de x es 5`. También podemos imprimir variables y operaciones matemáticas. Por ejemplo:

```Arduino
int num1 = 10;
int num2 = 3;
Serial.print("El resultado de la operación es ");
Serial.println(num1 * num2);
```

Esto imprimirá en la consola: `El resultado de la operación es 30`.

## Inmersión Profunda:

La impresión de salida de depuración ha existido desde los primeros días de la programación y sigue siendo una herramienta útil y predilecta para los programadores. Sin embargo, también existen otras formas de depuración, como usar puntos de interrupción y la creación de pruebas unitarias.

En Arduino, además de usar `Serial.print()`, también podemos usar `Serial.write()` para imprimir datos en formato binario. Además, también podemos cambiar la velocidad de transmisión de datos a través del Puerto Serial de Arduino con la función `Serial.begin()`.

## Ver también:

- [Documentación de Arduino sobre la impresión de datos de depuración](https://www.arduino.cc/en/Tutorial/Debugging)
- [Guía de depuración de Arduino por Adafruit](https://learn.adafruit.com/debugging-arduino-circuitpython/code-debugging-with-arduino)
- [Video tutorial de depuración de Arduino en YouTube](https://www.youtube.com/watch?v=kW24J_zEiMw)