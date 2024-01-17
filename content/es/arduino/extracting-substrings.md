---
title:                "Extrayendo subcadenas"
html_title:           "Arduino: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Extraer subcadenas es una técnica utilizada en programación para obtener una parte específica de una cadena de texto más grande. Los programadores lo hacen para obtener datos específicos de una cadena y utilizarlos en su código.

## Cómo hacerlo:
A continuación, se muestran dos ejemplos de cómo extraer subcadenas en Arduino. El primer ejemplo muestra cómo obtener una subcadena a partir de una cadena dada. El segundo ejemplo muestra cómo obtener una subcadena entre dos índices especificados.

```
// Ejemplo 1
String cadena = "Hola Mundo";
String subcadena = cadena.substring(5, 10); // Esto devuelve "Mundo"

// Ejemplo 2
String cadena = "123456789";
String subcadena = cadena.substring(2); // Esto devuelve "3456789"
```

## Profundizando:
Esta técnica se ha utilizado durante mucho tiempo en programación, incluso antes de la llegada de Arduino. Sin embargo, también hay alternativas como la función ```strtok()``` en C que realiza tareas similares. En cuanto a su implementación en Arduino, la función ```substring()``` pertenece a la clase String de la librería Arduino, por lo que puede ser utilizada en proyectos sin necesidad de importar librerías adicionales.

## Ver también:
- [Documentación oficial de la función ``substring()`` de Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Tutorial sobre cómo extraer subcadenas en Arduino (en inglés)](https://www.tutorialspoint.com/arduino/arduino_strings.htm)
- [Otra alternativa para extraer subcadenas en Arduino utilizando la función ```indexOf()```](https://www.arduino.cc/reference/en/language/functions/string/character-functions/indexof/)