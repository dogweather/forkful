---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Convertir una cadena a minúsculas en Arduino 

## ¿Qué es y por qué?

El proceso de convertir una cadena a minúsculas implica transformar todos los caracteres alfabéticos del texto en su equivalente en letra minúscula. Los programadores lo hacen para normalizar los datos, lo que permite efectuar comparaciones uniformes entre las cadenas de texto.

## ¿Cómo hacerlo?

Aquí tienes un ejemplo de cómo puedes utilizar la función `toLowerCase()` en Arduino para convertir una cadena a minúsculas.

```Arduino
String str = "¡Hola Mundo!";
str.toLowerCase();
Serial.begin(9600);
Serial.println(str);
```

En la Serial Monitor verás la salida: `¡hola mundo!`. Nota cómo la cadena original `¡Hola Mundo!` se ha transformado en `¡hola mundo!`.

## Profundizando

### Contexto histórico
Arduino es un lenguaje de programación basado en Wiring, que, a su vez, toma características de Processing. La función `toLowerCase()`, al igual que muchas otras funciones de cadena, se hereda de estas raíces.

### Alternativas
Antes de la introducción de `toLowerCase()`, los programadores usaban técnicas de manipulación de caracteres individuales para convertir cadenas a minúsculas.

```Arduino
char str[] = "¡Hola Mundo!";
for(int i = 0; str[i]; i++){
  str[i] = tolower((unsigned char) str[i]);
}
```
Este código produce el mismo resultado, pero el método `toLowerCase()` es mucho más cómodo de usar.

### Detalles de implementación
La función `toLowerCase()` recorre la cadena, convirtiendo cada carácter a minúsculas. Ignora los caracteres no alfabéticos y es insensible a los símbolos de acentuación en las letras mayúsculas.

## Ver también

Para más detalles, consulta la referencia de Arduino String:
* [Funciones toLowerCase() y toUpperCase()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
* [Arduino String](https://www.arduino.cc/reference/en/language/variables/data-types/string/) para funciones relacionadas y más información.
* [Manejo de cadenas en C++](http://www.cplusplus.com/reference/string/string/) para una perspectiva más amplia.