---
title:                "C: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué capitalizar una cadena en C?

La capitalización de una cadena es un proceso común en cualquier programa que maneje texto. Al capitalizar una cadena, se cambia la primera letra de cada palabra a mayúscula y se convierten el resto de las letras en minúsculas. Esto es especialmente útil en casos donde se necesite imprimir o mostrar datos al usuario de forma más legible.

## Cómo hacerlo:

Para capitalizar una cadena en C, existen varias formas de lograrlo. La manera más sencilla es utilizando la función `toupper()` que se encuentra en la biblioteca estándar `<ctype.h>`. Esta función convierte un carácter en mayúscula, por lo que se puede aplicar iterativamente a cada carácter de la cadena. A continuación, se muestra un ejemplo de código utilizando esta función:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
  char str[] = "hola, esta es una cadena a capitalizar.";
  int i = 0;

  while (str[i]) {
    str[i] = toupper(str[i]);
    i++;
  }

  printf("%s", str);

  return 0;
}
```

El resultado de este programa sería: `HOLA, ESTA ES UNA CADENA A CAPITALIZAR.`

Otra opción es utilizar la función `strtok()` que se encuentra en `<string.h>`, la cual permite dividir una cadena en "tokens" (trozos) utilizando un caracter delimitador. Se puede utilizar esto junto con la función `toupper()` para capitalizar cada palabra en la cadena. A continuación, se muestra un ejemplo:

```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main() {
  char str[] = "hola, esta es una cadena a capitalizar.";
  char delimitador[] = " ";
  char *token;

  // obtener el primer token
  token = strtok(str, delimitador);

  // seguir obteniendo tokens hasta el final de la cadena
  while (token != NULL) {
    token[0] = toupper(token[0]); // capitalizar primera letra
    printf("%s ", token); // imprimir token
    token = strtok(NULL, delimitador); // obtener siguiente token
  }

  return 0;
}
```

El resultado de este programa sería: `Hola, Esta Es Una Cadena A Capitalizar.`

Ambos ejemplos utilizan funciones básicas de C, por lo que son compatibles con la mayoría de compiladores.

## Profundizando en la capitalización de cadenas

Si se desea un control más específico sobre cómo se capitalizan las cadenas, se pueden utilizar otras funciones como `isalpha()` y `isblank()` de `<ctype.h>`, que permiten identificar si un carácter es una letra o un espacio en blanco, respectivamente. Esto puede ser útil en casos donde se quieran mantener ciertas palabras o símbolos en minúsculas, como en el caso de títulos de libros o nombres propios.

También se pueden utilizar algoritmos más complejos para capitalizar una cadena, como por ejemplo teniendo en cuenta excepciones y reglas gramaticales. Esto puede ser útil en proyectos de lenguaje natural o traductores.

## Ver también
- [Documentación de toupper() en C](https://www.tutorialspoint.com/c_standard_library/c_function_ctype_h_toascii.htm)
- [Documentación de strtok() en C](https://www.tutorialspoint.com/c_standard_library/c_function_strtok.htm)
- [Tutorial de C de W3Schools](https://www.w3schools.in/c-tutorial/)