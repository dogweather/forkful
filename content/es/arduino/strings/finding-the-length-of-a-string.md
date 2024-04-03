---
date: 2024-01-20 17:46:44.369600-07:00
description: "How to: (C\xF3mo hacerlo:) ."
lastmod: '2024-03-13T22:44:59.325460-06:00'
model: gpt-4-1106-preview
summary: .
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
