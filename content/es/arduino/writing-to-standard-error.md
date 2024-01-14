---
title:    "Arduino: Escritura en el error estándar"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué escribir en el error estándar en Arduino

Escribir en el error estándar en Arduino puede ser una herramienta muy útil para los programadores. Al enviar mensajes a este canal, podemos depurar nuestro código y detectar posibles errores o fallos en el funcionamiento de nuestro programa.

## Cómo hacerlo

Para escribir en el error estándar en Arduino, podemos utilizar la función `Serial.write()`. Esta función nos permite enviar datos a través del puerto serial al ordenador, donde podremos verlos en el monitor serial. Veamos un ejemplo de cómo utilizar esta función:

```Arduino
void setup() {
  Serial.begin(9600); // Iniciamos la comunicación serial a una velocidad de 9600 baudios
}

void loop() {
  int sensorValue = analogRead(A0); // Leemos un valor del pin analógico A0
  Serial.write(sensorValue); // Enviamos el valor al monitor serial
  delay(1000); // Esperamos un segundo antes de repetir el proceso
}
```
Al compilar y cargar este código en Arduino, podremos observar en el monitor serial los valores que se van leyendo del pin A0. Sin embargo, es importante recordar que la función `Serial.write()` solo envía datos en formato binario, por lo que debemos convertir nuestros datos a este formato antes de enviarlos.

## Profundizando en la escritura al error estándar

La función `Serial.write()` es solo una de las formas en las que podemos enviar datos al error estándar en Arduino. También podemos utilizar la función `Serial.print()`, que nos permite enviar datos en formato ASCII, lo que resulta más sencillo para leer los mensajes en el monitor serial.

Además, es importante tener en cuenta que podemos utilizar la escritura al error estándar en conjunto con otras técnicas de depuración, como el uso de variables debug o la impresión de valores por el puerto serial. Esto nos permite tener un mayor control y comprensión del funcionamiento de nuestro código mientras lo desarrollamos.

## Ver también

- `Serial.write()` en la documentación de Arduino: https://www.arduino.cc/reference/en/language/functions/communication/serial/write/
- Más sobre la comunicación serial en Arduino: https://www.arduino.cc/reference/en/language/functions/communication/serial/