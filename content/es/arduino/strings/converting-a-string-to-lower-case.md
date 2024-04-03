---
date: 2024-01-20 17:37:55.078611-07:00
description: "Convertir un texto a min\xFAsculas es cambiar todas las letras de una\
  \ cadena a su versi\xF3n en min\xFAscula. Lo hacemos para unificar el texto, facilitando\
  \ la\u2026"
lastmod: '2024-03-13T22:44:59.321686-06:00'
model: gpt-4-1106-preview
summary: "Convertir un texto a min\xFAsculas es cambiar todas las letras de una cadena\
  \ a su versi\xF3n en min\xFAscula."
title: "Conversi\xF3n de una cadena de texto a min\xFAsculas"
weight: 4
---

## Cómo Hacerlo:
El siguiente código convierte un `String` en Arduino a minúsculas. Usamos la función `toLowerCase()`, así de simple.

```Arduino
String texto = "¡Hola, Mundo!";
texto.toLowerCase();
Serial.begin(9600);
Serial.println(texto); // Imprime: ¡hola, mundo!
```

## Inmersión Profunda
Historia: La función `toLowerCase()` no es única de Arduino, existe en muchos lenguajes de programación. Fue creada para manejar datos de texto de manera más flexible.

Alternativas: Podrías recorrer cada carácter y convertirlo individualmente a minúscula, pero ¿para qué complicarse? `toLowerCase()` es directo y eficiente.

Detalles de Implementación: `toLowerCase()` modifica el objeto `String` sobre el que es llamado. No necesita de memoria adicional para almacenar la cadena resultante, haciendo que sea una operación en el lugar, que es eficiente en términos de memoria.

## Ver También
- Documentación oficial de Arduino sobre 'String': [Arduino Reference: String](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- Tutorial sobre cadenas en Arduino, incluyendo manipulación de texto: [Arduino String Tutorial](https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringAppendOperator)
- Para profundizar en cómo se implementan las operaciones con `String` en C++ (el lenguaje subyacente de Arduino): [C++ strings](http://www.cplusplus.com/reference/string/string/)
