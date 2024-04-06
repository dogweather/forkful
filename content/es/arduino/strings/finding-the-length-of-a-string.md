---
date: 2024-01-20 17:46:44.369600-07:00
description: "How to: (C\xF3mo hacerlo:) Antiguamente se usaban arreglos de caracteres\
  \ en C, terminados con un car\xE1cter nulo `'\\0'`, para calcular la longitud con\u2026"
lastmod: '2024-04-05T22:51:13.048573-06:00'
model: gpt-4-1106-preview
summary: "(C\xF3mo hacerlo:) Antiguamente se usaban arreglos de caracteres en C, terminados\
  \ con un car\xE1cter nulo `'\\0'`, para calcular la longitud con funciones como\
  \ `strlen()`."
title: Calculando la longitud de una cadena
weight: 7
---

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
