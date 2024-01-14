---
title:                "Arduino: Conversión de una cadena a minúsculas."
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

Si alguna vez has necesitado manipular una cadena de texto en tu proyecto de Arduino, es posible que hayas notado que algunas funciones solo funcionan correctamente con letras en minúscula. Por lo tanto, convertir una cadena de texto a minúsculas puede ser una habilidad muy útil a tener en tu caja de herramientas de programación de Arduino.

## Cómo hacerlo

Para convertir una cadena de texto a minúsculas en Arduino, necesitamos utilizar la función `toLowerCase()`. Esta función toma todos los caracteres de la cadena y los convierte en minúsculas. Veamos un ejemplo:

```arduino
String texto = "HOLA MUNDO";
String textoMinusculas = texto.toLowerCase();
Serial.println(textoMinusculas);
```

El resultado de este código será "hola mundo" en la consola serie. Como se puede ver, la función `toLowerCase()` es muy sencilla de utilizar.

## Profundizando

Aunque la función `toLowerCase()` es muy útil para convertir cadenas a minúsculas en Arduino, es importante destacar que esta función solo funciona con caracteres básicos como letras y números. No funciona correctamente con caracteres acentuados o símbolos especiales.

Si necesitas trabajar con estos tipos de caracteres en tus cadenas de texto, es recomendable utilizar una librería externa específica para conversiones de cadenas. Por ejemplo, la librería "Arduino String Object Utils" proporciona muchas funciones útiles para manipular cadenas de texto, incluyendo la conversión a minúsculas.

## Ver también

- [Librería Arduino String Object Utils](https://github.com/madsci1016/Arduino-String-Object-Utils)
- [Tutorial sobre cómo utilizar la función toLowerCase()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)