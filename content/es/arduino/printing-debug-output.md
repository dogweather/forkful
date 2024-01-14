---
title:    "Arduino: Imprimir salida de depuración"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué
¿Alguna vez te has quedado atascado en un problema de programación en Arduino y no sabes qué está pasando? La impresión de la salida de depuración puede ser una herramienta útil para ayudarte a entender lo que está pasando en tu código y encontrar posibles errores.

## Cómo hacerlo
Para imprimir la salida de depuración en Arduino, puedes usar la función `Serial.println()`, que imprimirá una línea seguida de un salto de línea. Por ejemplo:

```
Arduino 1: void setup() {
   Serial.begin(9600);
}

void loop() {
   Serial.println("Este es un ejemplo de salida de depuración");
   delay(1000);
}
```

Esto imprimirá la cadena "Este es un ejemplo de salida de depuración" cada segundo en el monitor serie de Arduino. También puedes usar `Serial.print()` para imprimir sin un salto de línea, o `Serial.write()` para imprimir datos binarios.

## Inmersión profunda
Imprimir la salida de depuración no solo puede ayudarte a encontrar errores en tu código, también puede ser útil para monitorear variables y valores en tiempo real mientras tu programa se está ejecutando. Además, puedes usar `Serial.read()` para leer los datos que ingresan a través del puerto serie y usarlos en tu código.

Otra forma de imprimir la salida de depuración es utilizar un módulo LCD para imprimir información en una pantalla. Esto puede ser útil si no tienes acceso al monitor serie o si deseas mostrar información en formato visual.

## Ver también
- [Tutorial sobre salida de depuración en Arduino](https://www.arduino.cc/en/Tutorial/SerialPrint)
- [Documentación oficial de Arduino sobre la librería serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Tutorial de uso de LCD en Arduino](https://www.arduino.cc/en/Tutorial/LiquidCrystalDisplay)