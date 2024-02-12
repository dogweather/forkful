---
title:                "Conversión de una cadena de texto a minúsculas"
aliases:
- /es/arduino/converting-a-string-to-lower-case/
date:                  2024-01-20T17:37:55.078611-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversión de una cadena de texto a minúsculas"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Convertir un texto a minúsculas es cambiar todas las letras de una cadena a su versión en minúscula. Lo hacemos para unificar el texto, facilitando la comparación y procesamiento de datos sin preocuparnos por las diferencias entre mayúsculas y minúsculas.

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
