---
date: 2024-01-20 17:55:31.103045-07:00
description: "Leer argumentos de la l\xEDnea de comando permite que tus programas\
  \ de Arduino reactiven a la entrada del usuario cuando arrancan. Los programadores\
  \ usan\u2026"
lastmod: '2024-03-13T22:44:59.348932-06:00'
model: gpt-4-1106-preview
summary: "Leer argumentos de la l\xEDnea de comando permite que tus programas de Arduino\
  \ reactiven a la entrada del usuario cuando arrancan."
title: "Lectura de argumentos de l\xEDnea de comandos"
weight: 23
---

## Qué y Por Qué?
Leer argumentos de la línea de comando permite que tus programas de Arduino reactiven a la entrada del usuario cuando arrancan. Los programadores usan esto para personalizar comportamientos sin tener que cambiar el código.

## Cómo:
Arduino no maneja argumentos de línea de comandos como lo haría un programa común de terminal ya que se comunica principalmente a través de su puerto serial después de haber sido programado. Pero puedes simular esta funcionalidad a través de la comunicación serial. Aquí hay un ejemplo:

```cpp
void setup() {
  // Inicia la comunicación serial
  Serial.begin(9600);
}

void loop() {
  // Revisa si hay datos para leer
  if (Serial.available() > 0) {
    // Lee la línea de comando entrante
    String command = Serial.readStringUntil('\n');

    // Ejecuta alguna acción basada en el comando
    if (command == "LED_ON") {
      digitalWrite(LED_BUILTIN, HIGH); // Enciende LED
      Serial.println("LED encendido!");
    } 
    else if (command == "LED_OFF") {
      digitalWrite(LED_BUILTIN, LOW); // Apaga LED
      Serial.println("LED apagado!");
    }
  }
}
```
Envías comandos al Arduino usando un monitor serial, el cual encontrarás en tu IDE de Arduino, no desde un terminal de línea de comando.

## Inmersión Profunda
Historia: Los argumentos de la línea de comando vienen de los días de los primeros sistemas operativos, donde la interacción con un programa era textual. Arduino hereda este principio básico a través del puerto serial para la interactividad en tiempo de ejecución.

Alternativas: Algunos dispositivos permiten leer argumentos de configuración almacenados en EEPROM o en ficheros en tarjetas SD al arrancar.

Detalles de Implementación: `Serial.available()` y `Serial.readStringUntil()` son funciones clave. La primera verifica si hay datos disponibles para leer, mientras que la segunda lee los datos hasta que encuentra un carácter de nueva línea.

## Ver También
- Documentación oficial del Serial de Arduino: [Arduino Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- Tutorial sobre cómo usar el almacenamiento EEPROM en Arduino: [Arduino EEPROM](https://www.arduino.cc/en/Tutorial/LibraryExamples/EEPROMWrite)
- Guía para leer archivos de una tarjeta SD con Arduino: [Arduino SD Card](https://www.arduino.cc/en/reference/SD)
