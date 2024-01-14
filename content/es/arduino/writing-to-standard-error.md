---
title:                "Arduino: Escribiendo a error estándar"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué escribir a la salida de error estándar en Arduino

Si eres nuevo en la programación de Arduino, es posible que hayas escuchado sobre la salida de error estándar, también conocida como "stderr". Pero, ¿por qué deberías preocuparte por ello? La respuesta es simple: escribir a la salida de error estándar es una forma útil de depurar y solucionar problemas en tu código.

## Cómo escribir a la salida de error estándar en Arduino

Para escribir a la salida de error estándar en Arduino, puedes utilizar la función "Serial.println()" y especificar "Serial", "USB" o "stderr" como parámetro. Aquí hay un ejemplo de cómo hacerlo:

```Arduino
Serial.println("¡Este es un mensaje de error!");
```

La salida de este código se enviará a tu consola de depuración o entorno de desarrollo integrado, lo que te permitirá ver los mensajes de error mientras ejecutas tu código.

## Profundizando en la escritura a la salida de error estándar

Escribir a la salida de error estándar es especialmente útil en situaciones en las que no se puede utilizar la salida serial, como en casos de errores en tiempo de ejecución. También puede ser útil cuando se necesita depurar múltiples líneas de código y la salida serial se llena con demasiada información.

Recuerda que escribir a la salida de error estándar no es la única forma de depurar y solucionar problemas en tu código de Arduino. Siempre es una buena práctica utilizar diferentes herramientas y métodos para garantizar la eficiencia y funcionalidad de tu código.

## Ver también
- [Documentación oficial de Arduino sobre la función Serial.println()](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Artículo sobre la depuración en Arduino](https://www.open-electronics.org/debugging-arduino-projects/)
- [Otra forma de depurar en Arduino: el LED de depuración](https://create.arduino.cc/projecthub/AnirbanS2297/debugging-arduino-projects-enhanced-version-07a62f)