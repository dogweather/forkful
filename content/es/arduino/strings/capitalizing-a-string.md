---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:57.008840-07:00
description: "C\xF3mo hacerlo: Arduino, principalmente conocido por interactuar con\
  \ hardware, tambi\xE9n incluye capacidades b\xE1sicas de manipulaci\xF3n de cadenas\
  \ a trav\xE9s de su\u2026"
lastmod: '2024-03-13T22:44:59.317952-06:00'
model: gpt-4-0125-preview
summary: "Arduino, principalmente conocido por interactuar con hardware, tambi\xE9\
  n incluye capacidades b\xE1sicas de manipulaci\xF3n de cadenas a trav\xE9s de su\
  \ objeto `String`."
title: Capitalizando una cadena de texto
weight: 2
---

## Cómo hacerlo:
Arduino, principalmente conocido por interactuar con hardware, también incluye capacidades básicas de manipulación de cadenas a través de su objeto `String`. Sin embargo, carece de una función directa de `capitalize` vista en lenguajes de programación de alto nivel. Por lo tanto, implementamos la capitalización iterando sobre una cadena y aplicando transformaciones de mayúsculas y minúsculas.

Aquí hay un ejemplo básico sin usar bibliotecas de terceros:

```cpp
String capitalizeString(String input) {
  if (input.length() == 0) {
    return ""; // Devuelve una cadena vacía si la entrada está vacía
  }
  input.toLowerCase(); // Convierte toda la cadena a minúsculas primero
  input.setCharAt(0, input.charAt(0) - 32); // Capitaliza el primer carácter
  
  // Capitaliza las letras que siguen a un espacio
  for (int i = 1; i < input.length(); i++) {
    if (input.charAt(i - 1) == ' ') {
      input.setCharAt(i, input.charAt(i) - 32);
    }
  }
  return input;
}

void setup() {
  Serial.begin(9600);
  String testStr = "hello arduino world";
  String capitalizedStr = capitalizeString(testStr);
  Serial.println(capitalizedStr); // Salida: "Hello Arduino World"
}

void loop() {
  // Bucle vacío
}
```

Este fragmento de código define una función `capitalizeString` que primero convierte toda la cadena a minúsculas para estandarizar su caso. Luego capitaliza el primer carácter y cualquier carácter que siga a un espacio, capitalizando efectivamente cada palabra en la cadena de entrada. Tenga en cuenta que esta implementación rudimentaria asume la codificación de caracteres ASCII y puede necesitar ajustes para el soporte completo de Unicode.

Actualmente, no hay bibliotecas de terceros ampliamente adoptadas específicamente para la manipulación de cadenas en el ecosistema de Arduino, principalmente debido a su enfoque en la interacción con hardware y la eficiencia. Sin embargo, el ejemplo proporcionado es una forma directa de lograr la capitalización de cadenas dentro del entorno de programación de Arduino.
