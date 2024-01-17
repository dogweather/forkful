---
title:                "Capitalizando una cadena"
html_title:           "Arduino: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

¡Hola, queridos lectores! ¿Alguna vez te has preguntado por qué algunas palabras en un programa están escritas en mayúsculas? Bueno, hoy te explicaré qué es capitalizar una cadena y porqué los programadores lo hacen.

## ¿Qué y porqué?

Capitalizar una cadena simplemente significa convertir todas las letras de una palabra o frase en mayúsculas. Los programadores hacen esto para facilitar la identificación de ciertas palabras en su código y también para cumplir con ciertos estándares de codificación. Además, algunas funciones y comandos en Arduino requieren que las cadenas estén escritas en mayúsculas para funcionar correctamente.

## Cómo:

```Arduino
String texto = "hola mundo";

// Utilizamos la función toUpperCase() para convertir todas las letras de la cadena en mayúsculas
texto.toUpperCase();

// Imprimimos la cadena original y la cadena con letras mayúsculas
Serial.println("Original: " + texto);
Serial.println("Mayúsculas: " + texto.toUpperCase());
```
Output:
```
Original: hola mundo
Mayúsculas: HOLA MUNDO
```

## Profundizando:

En el pasado, capitalizar una cadena era necesario debido a las limitaciones de los lenguajes de programación antiguos. Sin embargo, con los avances en la tecnología y los lenguajes de programación actuales, capitalizar una cadena se ha vuelto más una práctica personal para hacer el código más legible.

En lugar de utilizar la función toUpperCase(), también es posible utilizar la biblioteca <string.h> para capitalizar una cadena utilizando la función strtoupper().

En términos de implementación, la función toUpperCase() utiliza el código ASCII para convertir cada letra en su equivalente en mayúsculas. Es importante tener en cuenta que esta función solo funciona en letras y no afectará a otros caracteres.

## Ver también:

- Documentación de Arduino sobre la función toUpperCase(): https://www.arduino.cc/reference/en/language/functions/strings/stringtoupper/
- Más información sobre el código ASCII: https://www.arduino.cc/en/Reference/ASCIIchart