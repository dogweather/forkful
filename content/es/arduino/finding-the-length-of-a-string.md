---
title:    "Arduino: Encontrar la longitud de una cadena"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por Qué

En la programación de Arduino, es común trabajar con cadenas de caracteres o "strings". A menudo, es necesario conocer la longitud de una cadena para realizar diversas operaciones. En este artículo, aprenderemos cómo encontrar la longitud de una cadena en Arduino y la importancia de esta habilidad.

## Cómo

Para encontrar la longitud de una cadena en Arduino, podemos utilizar la función `strlen()`. Esta función toma una cadena como argumento y devuelve el número de caracteres en esa cadena. Veamos un ejemplo:

```Arduino
char myString[] = "¡Hola a todos!";
int length = strlen(myString);
Serial.println(length);
```
Este código imprimirá "14" en el monitor serie, ya que la cadena "¡Hola a todos!" tiene 14 caracteres.

Otra forma de encontrar la longitud de una cadena es utilizar un bucle `for` y la función `sizeof()`. Por ejemplo:

```Arduino
char myString[] = "¡Hola a todos!";
int length = 0;

for (int i = 0; i < sizeof(myString); i++) {
  if (myString[i] == '\0') { // encuentra el caracter nulo
    break;
  }
  length++;
}

Serial.println(length);
```

Este código también imprimirá "14" en el monitor serie, ya que el bucle recorrerá la cadena hasta encontrar el caracter nulo y contará cada caracter en el proceso.

## Profundizando

Es importante tener en cuenta que las funciones `strlen()` y `sizeof()` tienen diferentes comportamientos y es importante elegir la más adecuada para cada situación. La función `strlen()` cuenta solo los caracteres hasta el caracter nulo, mientras que `sizeof()` cuenta todos los caracteres, incluido el caracter nulo. También es importante entender que el caracter nulo (`\0`) es el que indica el final de la cadena.

Otro tema importante a considerar es el uso de la memoria. En ocasiones, es posible que no sepamos la longitud exacta de una cadena y necesitemos reservar memoria dinámicamente. En estos casos, es recomendable utilizar la función `strlen()` para evitar asignar más memoria de la necesaria.

## Ver También

- [Documentación de la función `strlen()` en la página de Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/)
- [Tutorial sobre cadenas en Arduino en español](https://aprendiendoarduino.wordpress.com/2017/05/01/manejo-de-cadenas-de-caracteres-en-arduino/)