---
title:                "Extracción de subcadenas"
html_title:           "Java: Extracción de subcadenas"
simple_title:         "Extracción de subcadenas"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Extraer subcadenas se refiere a la acción de obtener una parte de una cadena de texto más grande. Los programadores suelen hacer esto para manipular y trabajar con datos específicos dentro de una cadena de texto más grande.

## Cómo hacerlo:
Aquí hay algunos ejemplos de cómo extraer subcadenas en Java:

```Java
String str = "Hola mundo!";
// Obtener la subcadena "Hola"
String sub = str.substring(0,4);
// Obtener la subcadena "mundo!"
String sub2 = str.substring(5,11);
System.out.println(sub);
System.out.println(sub2);
```
**Salida:**
```
Hola
mundo!
```

## Profundizando:
Antiguamente, la manipulación de cadenas de texto era más compleja y los programadores tenían que utilizar funciones como `substring()` para extraer subcadenas. Sin embargo, ahora existen alternativas más directas y eficientes, como el método `split()` o el uso de expresiones regulares.

Además de poder especificar el inicio y el final de la subcadena, también se pueden utilizar otras formas de `substring()` para obtener diferentes resultados. Algunas de ellas son `substring(int beginIndex)` para obtener la subcadena desde el índice especificado hasta el final, y `substring(int beginIndex, int endIndex)` para obtener la subcadena desde el índice especificado hasta el índice anterior al especificado.

## Ver también:
- Documentación oficial de Java para el método `substring()`: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-
- Tutorial sobre cómo usar `substring()` en Java: https://www.w3schools.com/java/ref_string_substring.asp