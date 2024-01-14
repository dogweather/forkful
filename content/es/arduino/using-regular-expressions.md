---
title:                "Arduino: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Por qué utilizar expresiones regulares en Arduino?

Las expresiones regulares son una herramienta poderosa para procesar y manipular cadenas de texto en cualquier lenguaje de programación, incluyendo Arduino. Con la ayuda de expresiones regulares, podemos encontrar patrones específicos en una cadena de texto y realizar operaciones basadas en esos patrones, ahorrando tiempo y líneas de código.

## Cómo utilizar expresiones regulares en Arduino

Para utilizar expresiones regulares en Arduino, primero debemos incluir la librería "Regex.h". Luego, podemos crear un objeto Regex y pasarle como parámetro el patrón que queremos buscar en una cadena de texto.

```
#include <Regex.h> // Incluir la librería Regex

Regex miRegex("Hola"); // Crear un objeto Regex con el patrón "Hola"

if (miRegex.match("Hola Mundo")) { // Comprobar si la cadena de texto contiene el patrón
  Serial.println("Se encontró la palabra 'Hola'"); // Output: Se encontró la palabra 'Hola'
}
```

En el ejemplo anterior, utilizamos el método `match` para comprobar si la cadena de texto "Hola Mundo" contiene el patrón "Hola". Si es así, se imprime un mensaje en la consola serial.

## Profundizando en el uso de expresiones regulares

Además de buscar patrones simples como en el ejemplo anterior, las expresiones regulares también nos permiten realizar operaciones más complejas, como buscar varias palabras a la vez, sustituir palabras, o incluso utilizar caracteres especiales para hacer coincidir patrones específicos.

Por ejemplo, podemos buscar todas las palabras que comiencen con la letra "A" en una cadena de texto y contar cuántas veces aparecen utilizando el siguiente código:

```
Regex miRegex("[A]\\w+"); // El patrón "\w+" hace coincidir cualquier carácter alfanumérico después de la "A"

int contador = 0; // Inicializar un contador

while (miRegex.find("Ana ama los aviones y el arte")) { // Buscar todas las palabras que coincidan con el patrón
  contador++; // Incrementar el contador cada vez que se encuentre una coincidencia
}

Serial.println(contador); // Output: 3 (ya que hay 3 palabras que comienzan con "A")
```

También podemos utilizar caracteres especiales como `\d` para hacer coincidir dígitos numéricos, `\s` para hacer coincidir espacios en blanco, y `\b` para hacer coincidir límites de palabras.

## Ver también

- [Documentación de la librería Regex para Arduino](https://github.com/madsci1016/Arduino-EasyTransfer/blob/master/EasyTransfer/Regex.cpp)
- [Tutorial de expresiones regulares en Arduino](https://www.c-sharpcorner.com/article/arduino-regular-expression-a-simple-tutorial-to-get-started/)
- [Más ejemplos de expresiones regulares en Arduino](https://www.geeksforgeeks.org/regex-in-arduino-pattern-matching-in-arduino/)