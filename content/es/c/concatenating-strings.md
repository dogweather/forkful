---
title:                "C: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

Concatenar cadenas de caracteres es una tarea común en la programación en C. Esta técnica se utiliza para unir dos o más cadenas en una sola, lo que resulta útil para crear mensajes de salida personalizados o para manipular datos de texto. En este artículo, aprenderemos cómo concatenar cadenas de caracteres en el lenguaje de programación C y profundizaremos en cómo funciona esta operación.

## Cómo hacerlo

La concatenación de cadenas de caracteres se realiza mediante el uso de la función `strcat()` en C. Esta función toma dos cadenas de caracteres como argumentos y une la segunda cadena al final de la primera, sobrescribiendo el `\0` que indica el final de la primera cadena. A continuación, se muestra un ejemplo de código que concatena dos cadenas y muestra el resultado:

```C
#include <stdio.h>
#include <string.h>

int main() {
  char str1[20] = "¡Hola, ";
  char str2[] = "mundo!";
  
  strcat(str1, str2);
  printf("%s", str1);
  
  return 0;
}

// Output: ¡Hola, mundo!
```

Como se puede ver en el ejemplo, la función `strcat()` no solo une las cadenas, sino que también actualiza la cadena original para incluir el contenido de la segunda cadena. De esta manera, la cadena final incluye ambos textos.

También podemos usar la función `strncat()` para concatenar una cantidad específica de caracteres de la segunda cadena a la primera. Esta función toma un tercer argumento que indica la cantidad de caracteres que desea unir. A continuación, se muestra un ejemplo de código que utiliza `strncat()` para unir solo los dos primeros caracteres de la segunda cadena a la primera cadena:

```C
#include <stdio.h>
#include <string.h>

int main() {
  char str1[20] = "¡Hola, ";
  char str2[] = "mundo!";
  
  strncat(str1, str2, 2);
  printf("%s", str1);
  
  return 0;
}

// Output: ¡Hola, mu
```

## Profundizando

Además de las funciones `strcat()` y `strncat()`, también es importante tener en cuenta la función `strcpy()`. Esta función se utiliza para copiar una cadena a otra. Al igual que `strcat()`, puede sobrescribir la cadena original, por lo que es importante tener en cuenta la longitud de la cadena de destino y prevenir errores de desbordamiento.

También es importante tener en cuenta que la función `strcat()` solo funciona con cadenas que terminan con el carácter nulo `\0`. Si no es así, puede provocar comportamientos no deseados y errores en el programa.

Otra técnica común para concatenar cadenas es el uso del operador `+` con cadenas. Sin embargo, esto no es posible en C, ya que no es compatible con la sobrecarga de operadores. En su lugar, se debe utilizar la función `strcat()` para unir cadenas de caracteres.

## Ver también

- [Documentación de la función `strcat()` en C](https://www.geeksforgeeks.org/strcat-function-in-c/)
- [Ejemplos de concatenación de cadenas en C](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)
- [Tutorial de la función `strncat()` en C](https://www.programiz.com/c-programming/library-function/string.h/strncat)