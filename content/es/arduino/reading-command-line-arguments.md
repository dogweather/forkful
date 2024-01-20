---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Leer argumentos de línea de comandos significa procesar la información que el usuario proporciona al programa al ejecutarlo. Facilita el uso interactivo de programas y scripts, permitiendo al usuario modificar el comportamiento del programa sin cambiar su código.

## Cómo hacerlo:
Importante mencionar que Arduino no trabaja con líneas de comando, ya que es un microcontrolador y no un sistema operativo. Aún así, podemos simular esta acción mediante la lectura de datos desde el puerto serial. Aquí hay un breve ejemplo de cómo emular este comportamiento:

```Arduino
String comando;

void setup() {
  Serial.begin(9600);
}

void loop() {
  if (Serial.available() > 0)
  {
    comando = Serial.readString();
    if (comando == "encender_led")
    {
      digitalWrite(LED_BUILTIN, HIGH);
      Serial.println("LED encendido");
    }
    else if (comando == "apagar_led")
    {
      digitalWrite(LED_BUILTIN, LOW);
      Serial.println("LED apagado");
    }
  }
}
```
En este caso, si escribimos "encender_led" o "apagar_led" en el monitor serial, controlaremos el estado del LED incorporado.

## Hurgando más Fondo
Históricamente, la lectura de argumentos de línea de comandos se remonta a los primeros días del desarrollo de software, donde los usuarios interactuaban con programas a través de terminales de texto.

Aunque Arduino no soporta directamente los argumentos de línea de comandos, hay alternativas como el uso del puerto serie, o, en arquitecturas más avanzadas como el ESP32, mediante la construcción de una interfaz de línea de comandos personalizada.

El método anterior es bastante rudimentario y no es equivalente a una verdadera lectura de argumentos de línea de comandos, como tendría lugar en un entorno de sistema operativo. Entender esto es crucial para manejar de la mejor manera posible la interacción con el usuario.

## Ver También
1. Universo Arduino - Introducción a la programación de Arduino: [link](https://www.universoarduino.com)
2. Arduino - Referencia de la función Serial.readString(): [link](https://www.arduino.cc/reference/en/language/functions/communication/serial/readstring/)
3. ESP32 - Desarrollo de una interfaz de línea de comandos: [link](https://randomnerdtutorials.com/esp32-cmd-command-line-interface-uart-tutorial/)