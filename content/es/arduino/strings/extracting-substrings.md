---
date: 2024-01-20 17:45:06.718289-07:00
description: "C\xF3mo: Extracting substrings is a common task in many programming\
  \ environments, and it's been a part of Arduino's String class for as long as the\
  \ String\u2026"
lastmod: '2024-04-05T21:54:00.670219-06:00'
model: gpt-4-1106-preview
summary: Extracting substrings is a common task in many programming environments,
  and it's been a part of Arduino's String class for as long as the String class has
  been around.
title: "Extracci\xF3n de subcadenas"
weight: 6
---

## Cómo:
```Arduino
String texto = "Hola mundo Arduino";
String subcadena;

// Extraer "Hola"
subcadena = texto.substring(0, 4);
Serial.println(subcadena); // Muestra: Hola

// Obtener "mundo"
subcadena = texto.substring(5, 10);
Serial.println(subcadena); // Muestra: mundo

// Tomar "Arduino" desde el final
subcadena = texto.substring(11);
Serial.println(subcadena); // Muestra: Arduino
```

## Inmersión Profunda
Extracting substrings is a common task in many programming environments, and it's been a part of Arduino's String class for as long as the String class has been around. The `substring()` method in Arduino returns a portion of the string, either from a start index to the end of the string or between a specified range of indices.

Hay alternativas a `substring()`, como usar `charAt()` para iterar a través de los caracteres o manipular `char` arrays directamente, pero estas técnicas pueden ser más complejas y menos intuitivas.

La implementación de `substring()` crea una nueva instancia de String, lo que significa que consume memoria adicional del Arduino. Hay que tener cuidado con esto, especialmente en proyectos donde la memoria es limitada.

## Ver También
- Documentación oficial de Arduino en Strings: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Tutorial de Arduino sobre manejo de Strings: https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringAdditionOperator
- Temas avanzados de gestión de memoria en Arduino: https://www.arduino.cc/en/Tutorial/BuiltInExamples/MemoryManagement
