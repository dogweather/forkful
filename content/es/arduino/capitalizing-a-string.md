---
title:                "Capitalizando una cadena de texto"
date:                  2024-01-19
html_title:           "Arduino: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Capitalizar una cadena significa convertir todas las letras de una palabra o frase a mayúsculas. Los programadores lo hacen para normalizar los datos para comparaciones o para mejorar la estética de la interfaz de usuario.

## Cómo Hacerlo:
En Arduino, aún no hay una función incorporada para capitalizar cadenas directamente, pero eso no nos detiene. Aquí tienes un ejemplo sencillo:

```Arduino
void setup() {
  Serial.begin(9600);
  String mensaje = "¡hola, mundo!";
  mensaje.toUpperCase();
  Serial.println(mensaje);  // Salida: "¡HOLA, MUNDO!"
}

void loop() {
  // No necesitamos nada aquí por ahora.
}
```

## Inmersión Profunda:
Originalmente, Arduino no se diseñó para manipulación avanzada de texto, se enfocaba más en interactuar con el hardware. La función `toUpperCase()` es fácil de usar, pero no es muy eficiente con la memoria, algo a tener en cuenta con Arduino.

Alternativas incluirían trabajar con arrays de caracteres (`char[]`) y modificar cada letra individualmente, aprovechando la tabla ASCII para realizar la conversión de minúsculas a mayúsculas, que es sumando o restando 32 a los valores ASCII de letras minúsculas.

Detalles de implementación: Al hacer `mensaje.toUpperCase()`, la cadena original se modifica en lugar de crear una nueva. Si necesitas conservar la original, tendrías que duplicarla antes de cambiarla.

## Ver También:
- Documentación oficial de Arduino sobre la clase String: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Tutorial sobre el manejo de cadenas en Arduino: https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringAdditionOperator
- Explicación de ASCII y su aplicación en Arduino: https://www.arduino.cc/en/Reference/ASCIIchart
