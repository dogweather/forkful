---
title:                "Arduino: Extrayendo subcadenas"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas puede ser una tarea útil en la programación de Arduino. Puede permitirnos manipular y trabajar con partes específicas de un texto, lo cual puede ser útil en diversas aplicaciones como, por ejemplo, en la manipulación de datos o en la creación de interfaces de usuario.

## Cómo

Para extraer subcadenas en Arduino, podemos utilizar la función `substring()` que nos permite especificar qué parte de una cadena queremos obtener. Veamos un ejemplo:

```Arduino
String frase = "¡Hola a todos!";
String saludo = frase.substring(0, 4);
Serial.println(saludo);
```

Este código imprimirá "¡Hola" en el monitor serial, ya que especificamos que queremos obtener los caracteres desde la posición 0 hasta la 4 de la cadena `frase`. Nota que la posición 0 es el primer caracter de la cadena, mientras que la posición 4 es el quinto caracter.

También podemos utilizar la función `indexOf()` para obtener la posición de un determinado caracter en una cadena. Por ejemplo:

```Arduino
String frase = "¡Hola a todos!";
int pos = frase.indexOf('a');
Serial.println(pos);
```

Este código imprimirá 6, ya que la primera posición en la que se encuentra la letra "a" en la cadena `frase` es en la posición 6.

## Deep Dive

La función `substring()` también nos permite obtener una subcadena a partir de una posición inicial hasta el final de la cadena. Por ejemplo:

```Arduino
String juan = "Juan Pérez";
String apellido = juan.substring(5);
Serial.println(apellido);
```

Este código imprimirá "Pérez", ya que especificamos que queremos obtener todos los caracteres a partir de la posición 5 hasta el final de la cadena `juan`.

Otra función útil es `substring()` junto con `indexOf()` para obtener una subcadena entre dos caracteres específicos. Por ejemplo:

```Arduino
String frase = "Siempre positivo";
String palabra = frase.substring(frase.indexOf("pos"), frase.indexOf("pos") + 6);
Serial.println(palabra);
```

Este código imprimirá "positivo", ya que especificamos que queremos obtener todos los caracteres desde la palabra "pos" hasta los siguientes 6 caracteres después de esta palabra.

## Ver además

Aquí hay algunos enlaces útiles para seguir aprendiendo sobre cómo extraer subcadenas en Arduino:

- [Documentación de la función `substring()` en la referencia de Arduino](https://www.arduino.cc/reference/es/language/variables/data-types/string/functions/substring/)
- [Tutorial de Arduino sobre cómo manipular cadenas de texto](https://www.arduino.cc/en/Tutorial/StringLength)
- [Video tutorial sobre cómo extraer subcadenas en Arduino](https://www.youtube.com/watch?v=XsuM4GQtJoo)

¡Ahora ya tienes las herramientas para empezar a jugar con subcadenas en tus proyectos de Arduino! Recuerda siempre probar y experimentar con diferentes casos para familiarizarte con estas funciones. ¡Diviértete programando!