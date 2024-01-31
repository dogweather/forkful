---
title:                "Escribiendo en el error estándar"
date:                  2024-01-19
simple_title:         "Escribiendo en el error estándar"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?
Escribir en el error estándar significa mandar mensajes de error a un flujo específico, para diferenciarlos de la salida regular. Los programadores lo usan para depurar y notificar errores sin mezclarlos con la salida de datos normal.

## Cómo hacerlo:
Arduino no tiene una función incorporada de error estándar como tal. Las advertencias y errores se pueden enviar al puerto serie para diferenciarlos del flujo principal.

```Arduino
void setup() {
  // Iniciar comunicación serie
  Serial.begin(9600);
}

void loop() {
  // Error simulado
  if (digitalRead(2) == HIGH) {
    // Enviar mensaje de error al puerto serie
    Serial.println("ERROR: El pin 2 está en HIGH");
  }
  // Realizar otras tareas...
}
```
Muestra de la salida:
```
ERROR: El pin 2 está en HIGH
```

## Detalles Internos:
Historia: Arduino fue diseñado para ser simple y para la educación, por eso su ambiente de desarrollo no enfatiza sobre conceptos como error estándar.

Alternativas: Podrías usar diferentes Serial (como `Serial1`, `Serial2`, etc. en algunos Arduinos) para separar mensajes.

Implementación: Puedes definir tu propio manejo de errores utilizando funciones personalizadas que envían mensajes a través del puerto serie o a otros dispositivos de salida.

## Ver También:
- Documentación de Arduino sobre puerto serie: https://www.arduino.cc/reference/en/language/functions/communication/serial/
- Información detallada sobre el flujo estándar de error en programación general: https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr)
