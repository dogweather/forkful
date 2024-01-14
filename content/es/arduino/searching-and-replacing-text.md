---
title:                "Arduino: Búsqueda y reemplazo de texto"
simple_title:         "Búsqueda y reemplazo de texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

##¿Por qué utilizar búsqueda y reemplazo de texto en programación con Arduino?

La búsqueda y reemplazo de texto es una habilidad esencial en la programación con Arduino, ya que nos permite buscar un texto específico dentro de una cadena de caracteres y reemplazarlo por otro. Esta técnica es muy útil para automatizar tareas y ahorrar tiempo en la escritura de código.

##Cómo hacer búsqueda y reemplazo de texto en Arduino

Para realizar búsqueda y reemplazo de texto en Arduino, podemos utilizar la función `replace` de la librería `String`. A continuación, se muestra un ejemplo de código utilizando esta función:

```
ArduinoString miCadena = "Hola mundo";
miCadena.replace("Hola", "Adiós");
Serial.println(miCadena); // Imprimirá "Adiós mundo"
```

En este ejemplo, la palabra "Hola" es reemplazada por "Adiós" en la cadena `miCadena`, y luego se imprime el resultado por el puerto serial.

##Profundizando en la búsqueda y reemplazo de texto

La función `replace` también nos permite reemplazar múltiples instancias de un texto en una cadena. Por ejemplo:

```
ArduinoString miCadena = "Hola hola hola";
miCadena.replace("hola", "adiós");
Serial.println(miCadena); // Imprimirá "adiós adiós adiós"
```

Además, podemos utilizar la función `replace` en conjunto con otras funciones de la librería `String`, como `indexOf` y `substring`, para reemplazar un texto específico en una posición determinada dentro de una cadena.

En algunas situaciones, puede ser más conveniente utilizar la función `replaceAll`, que realiza la misma tarea que `replace`, pero busca y reemplaza todas las instancias del texto en la cadena sin importar su posición.

##Ver también

- [Referencia de la función replace](https://www.arduino.cc/reference/en/language/functions/strings/stringobject/replace/)
- [Tutorial de búsqueda y reemplazo de texto en Arduino](https://www.instructables.com/id/Searching-and-Replacing-String-Character-inside-an/)
- [Librería String de Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/)