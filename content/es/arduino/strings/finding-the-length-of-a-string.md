---
date: 2024-01-20 17:46:44.369600-07:00
description: "Encontrar la longitud de una cadena significa saber cu\xE1ntos caracteres\
  \ contiene. Los programadores lo hacen para manipular texto con precisi\xF3n, como\u2026"
lastmod: '2024-02-25T18:49:55.797086-07:00'
model: gpt-4-1106-preview
summary: "Encontrar la longitud de una cadena significa saber cu\xE1ntos caracteres\
  \ contiene. Los programadores lo hacen para manipular texto con precisi\xF3n, como\u2026"
title: Calculando la longitud de una cadena
---

{{< edit_this_page >}}

## What & Why? (¿Qué y Por Qué?)
Encontrar la longitud de una cadena significa saber cuántos caracteres contiene. Los programadores lo hacen para manipular texto con precisión, como validar entradas o controlar la salida en pantallas.

## How to: (Cómo hacerlo:)
```Arduino
String texto = "¡Hola, Arduino!";
int longitud = texto.length();

void setup() {
  Serial.begin(9600);
  Serial.println("La longitud de la cadena es:");
  Serial.println(longitud);
}

void loop() {
  // Aquí no necesitamos nada.
}
```
Salida:
```
La longitud de la cadena es:
14
```

## Deep Dive (Inmersión Profunda)
Antiguamente se usaban arreglos de caracteres en C, terminados con un carácter nulo `'\0'`, para calcular la longitud con funciones como `strlen()`. En Arduino, la clase `String` viene con `.length()`, lo cual es mucho más directo. Pero ojo, abusar de `String` puede fragmentar la memoria. Alternativas incluyen usar `char` arrays y gestionar la memoria manualmente, que es más complejo pero puede ser más eficiente.

## See Also (Ver También)
- Documentación oficial de Arduino sobre `String`: [Arduino - StringObject](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- Tutorial de C++ sobre arreglos de caracteres (relevantes por su similitud al entorno de Arduino): [C++ Character Arrays](https://www.cplusplus.com/doc/tutorial/ntcs/)
