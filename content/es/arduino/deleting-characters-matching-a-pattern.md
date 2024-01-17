---
title:                "Eliminando caracteres que coinciden con un patrón."
html_title:           "Arduino: Eliminando caracteres que coinciden con un patrón."
simple_title:         "Eliminando caracteres que coinciden con un patrón."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

En la programación, borrar caracteres que coinciden con un patrón se refiere a eliminar ciertos caracteres dentro de una cadena de texto que se ajusten a un patrón específico. Los programadores a menudo lo hacen para limpiar y organizar datos, o para evitar que ciertos caracteres causen errores en su código.

## ¿Cómo hacerlo?

Para borrar caracteres que coinciden con un patrón en Arduino, podemos utilizar la función "replace()" que nos permite reemplazar todas las instancias de un carácter con otro en una cadena de texto. Aquí hay un ejemplo de cómo usarlo:

```Arduino
// Definimos una cadena de texto
String texto = "Hello $World$!";

// Eliminamos el símbolo "$" utilizando la función "replace()"
texto.replace("$", "");

// Imprimimos el resultado
Serial.println(texto); // Salida: Hello World!
```

## Profundizando

La eliminación de caracteres que coinciden con un patrón se ha vuelto cada vez más común con el auge del procesamiento de texto y el análisis de datos. Antes, los programadores tenían que recorrer manualmente cada carácter buscando el patrón deseado, pero ahora, con funciones como "replace()", se puede hacer de manera más eficiente.

Existen también otras formas de eliminar caracteres que coinciden con un patrón en Arduino, como utilizar expresiones regulares, que son secuencias de caracteres que definen un patrón de búsqueda. Sin embargo, estas pueden ser más complejas y requieren más conocimiento de programación.

En cuanto a la implementación, la función "replace()" en Arduino funciona mediante la modificación directa de la cadena de texto original, reemplazando las instancias del carácter dado con otro carácter provisto por el usuario.

## Ver también

- Documentación de Arduino sobre la función "replace()": https://www.arduino.cc/reference/es/language/variables/data-types/string/functions/replace/ 
- Tutorial sobre expresiones regulares en Arduino: https://diyprojects.io/arduino-string-regular-expression-find-replace-detect-match/