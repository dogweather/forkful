---
title:    "Arduino: Convirtiendo una cadena a minúsculas"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una cadena de texto a minúscula en Arduino?

A menudo, al utilizar una cadena de texto en un proyecto de Arduino, es necesario que todas las letras estén en minúscula para que el código funcione correctamente. Esto puede ser necesario para comparar la cadena con otra o para que el texto se muestre correctamente en una pantalla LCD, por ejemplo. Por lo tanto, es importante saber cómo convertir una cadena de texto a minúscula en Arduino.

## Cómo hacerlo

Para convertir una cadena de texto a minúscula en Arduino, podemos utilizar la función `toLower()` de la librería `String`. Esta función recorre la cadena y convierte cada letra a minúscula. Veamos un ejemplo:

```Arduino
#include <String.h>

String texto = "CONVERSIÓN A MINÚSCULA";
String textoMinusc = texto.toLower();

Serial.println(textoMinusc); // imprime "conversión a minúscula" en el monitor serial
```

En este ejemplo, creamos una cadena de texto llamada `texto` y la convertimos a minúscula utilizando la función `toLower()`. Luego, imprimimos la cadena resultante en el monitor serial y podemos ver que todas las letras están en minúscula.

## Profundizando en la conversión de cadenas de texto a minúscula

Es importante tener en cuenta que la función `toLower()` solo convierte letras mayúsculas a minúsculas. Si la cadena de texto contiene algún carácter especial o un número, estos no se modificarán con la función. Además, la función solo funciona con objetos de tipo String, no con arrays de caracteres.

También es posible utilizar un bucle `for` para recorrer la cadena de texto y convertir cada letra a minúscula utilizando la función `tolower()` de la librería `ctype.h`. Sin embargo, esto puede ser más complejo y menos eficiente que utilizar la función `toLower()`.

## Ver también

- [Documentación de la función `toLower()`](https://www.arduino.cc/reference/en/language/functions/string-functions/toupper/)
- [Ejemplo práctico de cómo convertir una cadena de texto a minúscula en Arduino](https://www.instructables.com/Converting-String-to-Lower-Case-in-Arduino/)
- [Preguntas frecuentes sobre el uso de cadenas de texto en Arduino](https://arduino.stackexchange.com/questions/3830/how-do-i-use-character-arrays-instead-of-strings)