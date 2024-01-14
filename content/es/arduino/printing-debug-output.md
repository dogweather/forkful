---
title:    "Arduino: Imprimiendo salida de depuración"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por qué

Cuando estás trabajando con Arduino, puede ser útil imprimir mensajes de depuración en la consola. Esto te permite ver qué está sucediendo dentro del código mientras se está ejecutando, lo que facilita la identificación de errores y el seguimiento del proceso del programa. Además, la impresión de mensajes de depuración puede ser una forma útil de monitorear el estado de tus componentes y sensores conectados.

## Cómo hacerlo

Arduino tiene una función incorporada para imprimir mensajes de depuración, llamada `Serial.print()`. Esta función toma un argumento, que puede ser un valor numérico, una cadena de texto o una variable, y lo imprime en la consola serial. Por ejemplo, si quieres imprimir el valor de una variable `contador` en tu código, puedes hacerlo de la siguiente manera:

```Arduino
Serial.print(contador);
```

También puedes combinar varias variables o valores en un solo mensaje de depuración, utilizando la función `Serial.println()`. Esta función es similar a `Serial.print()`, pero agrega un salto de línea al final del mensaje, lo que hace que cada mensaje se imprima en una línea separada en la consola serial. Por ejemplo:

```Arduino 
Serial.println("El valor del contador es: "); 
Serial.println(contador);
```

Este código imprimiría dos líneas en la consola serial: una con el texto "El valor del contador es:" y otra con el valor de la variable `contador`.

Otra forma de imprimir mensajes de depuración es utilizando la función `Serial.write()`. Esta función toma un arreglo de bytes como argumento, lo que te permite imprimir datos binarios en lugar de solo texto o valores. Puedes utilizar esta función para imprimir datos de sensores o cualquier otro tipo de información que no se pueda convertir fácilmente a una cadena de texto.

## Profundizando más

Además de las funciones mencionadas anteriormente, Arduino también tiene otras opciones para imprimir mensajes de depuración. Por ejemplo, puedes especificar el puerto serial en el que deseas imprimir utilizando la función `Serial.begin()`. También puedes controlar la velocidad de transmisión de los mensajes con la función `Serial.begin()`, lo que puede ser útil cuando necesitas imprimir grandes cantidades de datos.

También puedes utilizar el software de terminal serial de Arduino para ver los mensajes de depuración en lugar de la consola serial integrada en la placa. Esto puede ser especialmente útil si estás trabajando con una placa Arduino sin puertos USB.

Además, hay bibliotecas disponibles que te permiten personalizar la forma en que se imprimen los mensajes de depuración, como la biblioteca `DebugPrint`. Esta biblioteca te permite formatear tus mensajes de depuración con diferentes estilos y colores, haciéndolos más fáciles de leer y analizar.

## Ver también

- [Documentación oficial de Arduino sobre la función Serial.print()](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Tutorial de Adafruit sobre la impresión de mensajes de depuración con Arduino](https://learn.adafruit.com/arduino-lesson-5-the-serial-monitor/overview)
- [Biblioteca DebugPrint para personalizar los mensajes de depuración en Arduino](https://github.com/davetcc/DebugPrint)