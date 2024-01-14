---
title:                "Arduino: Imprimiendo salida de depuración"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Por qué imprimir la salida de depuración en Arduino?

La impresión de la salida de depuración es una práctica común en la programación de Arduino, ya que puede ser de gran ayuda para encontrar errores y solucionar problemas en tu código. Al imprimir la información del proceso de ejecución, puedes identificar posibles fallas y corregirlas de manera más eficiente. Además, puede proporcionar una visión general del funcionamiento de tu programa.

## Cómo imprimir la salida de depuración en Arduino

Para imprimir la salida de depuración en Arduino, utilizamos la función `Serial.println()`, que permite enviar datos a través del puerto serial a una computadora u otro dispositivo. Por ejemplo:

```
Arduino.println("La temperatura actual es: " + String(temperatura));
```

Este código imprimirá en pantalla la temperatura actual medida por el sensor en formato de texto. También puedes utilizar variables dentro de la función para imprimir datos en tiempo real.

## Profundizando en la impresión de salida de depuración

Además de la función `Serial.println()`, existen otras formas de imprimir la salida de depuración en Arduino, como `Serial.print()` y `Serial.write()`. También puedes utilizar el monitor serial en el IDE de Arduino para ver y guardar los datos de salida.

Es importante tener en cuenta que la impresión de la salida de depuración puede afectar al rendimiento de tu código, por lo que es recomendable desactivarla una vez que hayas finalizado la fase de depuración.

## Ver también
- [Tutorial de Arduino sobre la impresión de la salida de depuración](https://www.arduino.cc/en/Tutorial/Serial)
- [Documentación oficial de la función Serial.println()](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Ejemplos de impresión de salida de depuración en proyectos de Arduino](https://circuitdigest.com/microcontroller-projects/arduino-serial-print-behind-the-curtains)

¡Ahora ya sabes cómo imprimir la salida de depuración en tus programas de Arduino! Con esta herramienta, podrás depurar tu código de manera más efectiva y asegurarte de que todo funcione correctamente. ¡Sigue explorando y aprendiendo más sobre Arduino en nuestra sección de tutoriales y proyectos!