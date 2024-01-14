---
title:    "Arduino: Capitalizando una cadena"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué capitalizar una cadena de texto en Arduino

Si estás trabajando en un proyecto de Arduino que involucra la manipulación de texto, puede que te hayas preguntado cómo capitalizar una cadena de texto. Ya sea que quieras que tu texto se vea más estético o simplemente seguir ciertas convenciones de escritura, capitalizar una cadena de texto puede ser una tarea útil en tu codificación de Arduino. En esta entrada de blog, explicaremos cómo capitalizar una cadena de texto en Arduino y profundizaremos en cómo funciona este proceso.

## Cómo hacerlo

Para capitalizar una cadena de texto en Arduino, necesitarás utilizar la función `toUpperCase()` junto con un bucle `for`. Esta función convierte todos los caracteres de una cadena en mayúsculas. A continuación, se muestra un ejemplo de código que utiliza la función `toUpperCase()` para capitalizar una cadena y luego imprime el resultado en el monitor serial:
```
Arduino String texto = "hola amig o";
for (int i = 0; i < texto.length(); i++) {
  texto[i] = toUpperCase(texto[i]);
}
Serial.println(texto);
```
En este ejemplo, la palabra "hola" se convertirá en "HOLA". Sin embargo, es importante tener en cuenta que esta función no funciona para todos los caracteres, ya que algunos idiomas tienen caracteres que no tienen una versión en mayúsculas.

## Profundizando

En Arduino, las cadenas de texto se almacenan como un conjunto de caracteres en una matriz, y cada carácter tiene un valor numérico asociado. Las letras mayúsculas y minúsculas tienen valores numéricos diferentes, y es gracias a esto que la función `toUpperCase()` funciona correctamente. Al recorrer cada carácter y aplicar la función, se cambia su valor numérico al correspondiente en mayúscula.

Sin embargo, si necesitas capitalizar una cadena que contenga caracteres que no tienen una versión en mayúscula, como acentos o símbolos, tendrás que utilizar una función personalizada que incluya esas excepciones.

## Ver también

- La Guía de Referencia de Arduino para obtener más información sobre la función `toUpperCase()`: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/
- Una lista de caracteres y sus respectivos valores numéricos en Arduino: https://www.asciitable.com/
- Otros ejemplos y técnicas para trabajar con cadenas de texto en Arduino: https://www.arduino.cc/en/Tutorial/StringConstructors