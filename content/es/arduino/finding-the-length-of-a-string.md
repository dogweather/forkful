---
title:                "Calculando la longitud de una cadena"
date:                  2024-01-20T17:46:44.369600-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calculando la longitud de una cadena"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/finding-the-length-of-a-string.md"
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
