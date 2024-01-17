---
title:                "Encontrar la longitud de una cadena"
html_title:           "Arduino: Encontrar la longitud de una cadena"
simple_title:         "Encontrar la longitud de una cadena"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

En la programación, encontrar la longitud de una cadena (string) significa determinar la cantidad de caracteres que la conforman. Los programadores hacen esto para poder manipular y analizar mejor sus datos, ya que la longitud de una cadena puede variar y es importante conocerla para evitar errores en el código.

## Cómo:

```Arduino
// Ejemplo de código para encontrar la longitud de una cadena
String cadena = "Hola Mundo!";
int longitud = cadena.length();
Serial.println(longitud);

// Salida: 11
```

El código anterior crea una variable de tipo String llamada "cadena" con el valor "Hola Mundo!", luego utiliza el método "length()" para obtener la longitud de la cadena y finalmente imprime ese valor en el monitor serial.

## Profundizando:

- Este método fue introducido en el lenguaje de programación C en los años 70, pero ha sido adoptado por muchos otros lenguajes, incluyendo Arduino.
- Si necesitas encontrar la longitud de un array de caracteres (caracter string), puedes utilizar el método `strlen()`.
- Para obtener la longitud de una cadena en un lenguaje de programación que no tenga un método específico, puedes iterar a través de la cadena y contar los caracteres uno por uno.

## Ver también:

- Documentación oficial de Arduino sobre `String.length()`: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/length/
- Explicación detallada de cómo funciona `String.length()`: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/