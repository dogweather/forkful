---
title:                "Escritura en el error estándar"
html_title:           "Arduino: Escritura en el error estándar"
simple_title:         "Escritura en el error estándar"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Por qué hacer esto?

Escribir a la salida de error estándar puede ser útil para monitorear el comportamiento de su programa y detectar posibles errores. Además, puede ayudar a identificar problemas y mejorar la depuración de su código.

## Cómo hacerlo

Para escribir a la salida de error estándar en Arduino, se puede utilizar la función `Serial.write()` junto con el objeto `Serial` para comunicarse con el puerto serial. A continuación se muestra un ejemplo de cómo se vería esta función en un código:

```Arduino
void setup() {
  // Iniciamos la conexión con el puerto serial
  Serial.begin(9600);
}

void loop() {
  // Ejemplo de escritura a la salida de error estándar
  Serial.write("Este es un mensaje de error");
  delay(1000);
}
```

El resultado de este código se mostrará en la consola de Serial Monitor en el IDE de Arduino.

## Profundizando más

Para obtener más información sobre la escritura a la salida de error estándar en Arduino, se puede consultar la documentación oficial en el sitio web de Arduino o buscar ejemplos y tutoriales en línea. Además, también se pueden explorar otras funciones y métodos disponibles para imprimir en la consola de Serial Monitor, como `Serial.print()` y `Serial.println()`.

## Consulte también

- Documentación oficial de Arduino para escribir a la salida de error estándar: https://www.arduino.cc/reference/en/language/functions/communication/serial/write/
- Ejemplos y tutoriales en línea sobre la escritura a la salida de error estándar en Arduino: https://maker.pro/arduino/tutorial/how-to-use-standard-error-stream-on-an-arduino-board