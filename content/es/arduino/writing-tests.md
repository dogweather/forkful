---
title:                "Escribiendo pruebas"
html_title:           "Arduino: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir pruebas en Arduino?

Escribir pruebas en Arduino es una práctica importante que ayuda a garantizar que nuestro código funcione correctamente en todas las situaciones posibles. Las pruebas nos permiten detectar errores y solucionarlos antes de implementar nuestro código en hardware, lo que ahorra tiempo y frustraciones en el futuro.

## Cómo hacerlo

Antes de comenzar a escribir pruebas, es importante entender la estructura básica de un programa en Arduino. Normalmente, tenemos dos funciones: `setup()` y `loop()`. La función `setup()` se ejecuta una sola vez al principio del programa, mientras que la función `loop()` se ejecuta de forma repetida hasta que el Arduino se apaga.

Para crear una prueba, debemos seguir estos pasos:

1. Definir una función para nuestra prueba, por ejemplo `test_led()`.
2. Dentro de la función, utilizar la función `pinMode()` para configurar un pin como salida.
3. Utilizar la función `digitalWrite()` para encender o apagar el pin.
4. Utilizar la función `delay()` para esperar un corto período de tiempo.
5. Utilizar la función `digitalRead()` para leer el valor del pin y verificar si está en el estado deseado.
6. Imprimir en la consola serial el resultado de la prueba utilizando `Serial.println()`.

Aquí hay un ejemplo de una prueba que verifica si un LED se enciende y se apaga correctamente:

```Arduino
void test_led() {
  pinMode(LED_PIN, OUTPUT);
  digitalWrite(LED_PIN, HIGH); // Enciende el LED
  delay(500); // Espera medio segundo
  if (digitalRead(LED_PIN) == HIGH) { // Verifica si el LED está encendido
    Serial.println("Prueba superada"); // Imprime en la consola serial
  } else {
    Serial.println("Fallo en la prueba");
  }

  digitalWrite(LED_PIN, LOW); // Apaga el LED
  delay(500);
  if (digitalRead(LED_PIN) == LOW) { // Verifica si el LED está apagado
    Serial.println("Prueba superada");
  } else {
    Serial.println("Fallo en la prueba");
  }
}
```

## Profundizando en las pruebas

Escribir pruebas es una técnica de programación llamada "pruebas unitarias", que consiste en probar cada unidad de código de forma aislada. Esto nos permite encontrar errores específicos y corregirlos de manera eficiente. También nos promueve a escribir un código más modular y fácil de mantener.

En Arduino, podemos utilizar la librería `ArduinoUnit` para escribir nuestras pruebas de una manera más organizada. Esta librería proporciona funciones útiles para crear y ejecutar pruebas, así como para verificar resultados y manejar errores.

Para profundizar en el tema de las pruebas en Arduino, se recomienda investigar más sobre las pruebas unitarias y la librería `ArduinoUnit`.

## Ver también

- Documentación oficial de Arduino: https://www.arduino.cc/en/Guide/HomePage
- Tutoriales de pruebas en Arduino: https://www.arduino.cc/en/Tutorial/BuiltInExamples#basic
- Librería ArduinoUnit: https://github.com/mmurdoch/arduinounit