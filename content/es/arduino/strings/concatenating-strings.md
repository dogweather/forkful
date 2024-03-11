---
date: 2024-01-20 17:34:06.997156-07:00
description: "Concatenar cadenas significa juntar dos o m\xE1s textos en uno solo.\
  \ Los programadores lo hacen para construir mensajes, combinar datos y presentar\u2026"
lastmod: '2024-03-11T00:14:33.147688-06:00'
model: gpt-4-1106-preview
summary: "Concatenar cadenas significa juntar dos o m\xE1s textos en uno solo. Los\
  \ programadores lo hacen para construir mensajes, combinar datos y presentar\u2026"
title: "Concatenaci\xF3n de cadenas de texto"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Concatenar cadenas significa juntar dos o más textos en uno solo. Los programadores lo hacen para construir mensajes, combinar datos y presentar información de forma dinámica.

## Cómo:
```Arduino
void setup() {
  // Inicia comunicación serial
  Serial.begin(9600);
}

void loop() {
  // Define dos cadenas
  String saludo = "Hola, ";
  String nombre = "Mundo";

  // Concatena las cadenas
  String mensajeCompleto = saludo + nombre;

  // Envía el mensaje completo al puerto serial
  Serial.println(mensajeCompleto);

  // Espera un poco antes de repetir
  delay(2000);
}
```
Salida de muestra:
```
Hola, Mundo
```

## Inmersión Profunda
Concatenar cadenas de texto es un concepto antiguo que ya existía en lenguajes predecesores como C y Java. En Arduino, hay varias maneras de hacerlo. Puedes usar el operador `+` con el objeto `String` como en el ejemplo anterior. Alternativamente, puedes concatenar con la función `strcat()` para C-strings (arrays de caracteres), pero hay que tener cuidado con el tamaño del buffer. En el nivel de implementación, concatenar con el `String` de Arduino puede consumir más memoria RAM, algo a considerar en microcontroladores con recursos limitados.

## Ver También
- La documentación oficial de Arduino sobre la clase `String`: https://www.arduino.cc/reference/en/language/variables/data-types/string/
- Un tutorial sobre gestión de memoria en Arduino: https://learn.arduino.cc/tutorials/mkr-wifi-1010/handling-memory-well-on-mkr-wifi-1010
