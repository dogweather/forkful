---
title:    "Arduino: Utilizando expresiones regulares"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué utilizar expresiones regulares en Arduino
Las expresiones regulares son una herramienta muy útil para el manejo de texto en el lenguaje de programación de Arduino. Con ellas, puedes buscar y manipular patrones específicos de texto en tu código, ahorrando tiempo y simplificando la programación.

## Cómo utilizar expresiones regulares en Arduino
Para utilizar expresiones regulares en Arduino, primero debes importar la librería "Regex". Luego, puedes utilizar la función `match()` para buscar un patrón específico en una cadena de texto. Por ejemplo:

```Arduino
#include <Regex.h>

String texto = "Hola, mi nombre es Juan y tengo 25 años";
String patron = "([A-Z][a-z]+), tengo ([0-9]+) años";
MatchState ms;
ms.Target(texto);

if(ms.Match(patron))
{
  for(int i = 0; i < ms.Groups; i++)
  {
    Serial.println(ms.GetCapture(texto, i));
  }
}
```

Este código buscará en la cadena de texto la palabra "Juan" seguida por un número, y si lo encuentra, imprimirá "Juan" y "25" en el monitor serial.

## Profundizando en el uso de expresiones regulares
Las expresiones regulares utilizan una sintaxis particular para definir patrones de texto. Por ejemplo, los corchetes `[ ]` se utilizan para definir un conjunto de caracteres posibles, mientras que el punto `.` representa cualquier carácter. Además, existen operadores como el asterisco `*` y el signo de más `+` que te permiten buscar más de un carácter en un patrón.

Puedes encontrar una lista completa de los operadores y la sintaxis utilizada en expresiones regulares en [este enlace](https://regexr.com/).

## Ver también
- [Documentación oficial de la librería "Regex" en Arduino](https://arduino-regex.readthedocs.io/en/latest/index.html)
- [Tutorial de introducción a expresiones regulares en Arduino](https://www.arduino.cc/reference/en/language/functions/regular-expressions/)
- [Listado de operadores y sintaxis en expresiones regulares](https://regexr.com/)