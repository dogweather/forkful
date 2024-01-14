---
title:                "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías encontrar la longitud de una cadena en Arduino?

En la programación de Arduino, es común trabajar con cadenas de texto. Saber la longitud de una cadena es útil en muchos proyectos, como la manipulación de datos o la validación de entradas del usuario. Aprender a encontrar la longitud de una cadena en Arduino puede hacer que tus proyectos sean más eficientes y precisos.

## Cómo encontrar la longitud de una cadena en Arduino

Para encontrar la longitud de una cadena en Arduino, podemos utilizar la función `strlen()` que se encuentra en la librería `string.h`. Esta función toma una cadena como argumento y devuelve la cantidad de caracteres que contiene.

```Arduino
#include <string.h>

// Definir una cadena
char myString[] = "Hola mundo";

// Encontrar la longitud de la cadena
int length = strlen(myString);

// Imprimir la longitud en el monitor serie
Serial.println(length);
```

La salida de este código sería `10`, ya que la cadena "Hola mundo" contiene 10 caracteres, incluyendo el espacio en blanco.

## Un vistazo más profundo

La función `strlen()` cuenta los caracteres desde el primer caracter hasta el caracter nulo `\0`. Esto significa que si tenemos una cadena con un caracter nulo en algún lugar que no sea el final, la función devolverá un valor incorrecto.

Además, si nuestra cadena es una cadena de caracteres Unicode, debemos tener en cuenta que algunos caracteres ocupan más de un byte y pueden afectar a la longitud de la cadena.

Para asegurarnos de encontrar la longitud correcta de una cadena, podemos usar un bucle para recorrer la cadena hasta encontrar el caracter nulo y contar los caracteres a medida que avanzamos.

```Arduino
// Definir la cadena
char unicodeString[] = "Arduino ♥";

// Inicializar contador en 0
int length = 0;

// Recorrer la cadena hasta encontrar el caracter nulo
while (unicodeString[length] != '\0') {
    // Aumentar el contador en 1
    length++;
}

// Imprimir la longitud en el monitor serie
Serial.println(length);
```

En este ejemplo, la salida sería `9`, ya que la cadena contiene un total de 9 caracteres en lugar de 10 debido a la presencia del caracter Unicode ♥.

## Ver también

- Tutorial de Arduino sobre cadenas de texto: https://www.arduino.cc/reference/es/language/variables/data-types/string/
- Guía sobre cómo trabajar con cadenas de texto en Arduino: https://www.programming-electronics-diy.xyz/arduino-string-class-iterating-chars/