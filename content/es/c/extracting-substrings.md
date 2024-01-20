---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Extraer subcadenas es un proceso que nos permite obtener una secuencia de caracteres de una cadena en particular. Los programadores lo hacen para manipular y analizar secciones específicas de datos de texto.

## ¡Cómo hacerlo!

En C, podemos extraer subcadenas utilizando la función `strncpy()`. Aquí hay un ejemplo:

```C
#include<stdio.h>
#include<string.h>

int main() {
  char str[] = "Hola, Mundo!";
  char sub[5];

  strncpy(sub, str, 4);
  sub[4] = '\0';

  printf("%s\n", sub);

  return 0;
}
```  
La salida de este código será:

```
Hola
```
Este código copia los primeros 4 caracteres de `str` en `sub`.

## Inmersión Profunda

Históricamente, la función `strncpy()` ha sido parte del lenguaje C desde sus primeros días. Aunque es una función versátil, tiene algunas limitaciones. Si no se maneja correctamente, puede llevar a errores de desbordamiento del búfer y a problemas de terminación de cadena nula.

Como alternativas para `strncpy()`, se podría usar `snprintf()` o `memcpy()`. Ambos pueden manejar el límite de tamaño de la cadena del destino, protegiéndote contra desbordamientos de búfer.

En cuanto a su implementación, la función `strncpy()` copia los caracteres del buffer de origen al de destino uno a uno hasta que ha copiado el número especificado de caracteres.

## Ver También

Si esta guía te fue útil, quizás quieras explorar temas relacionados. Aquí te dejamos un par de recursos útiles:

- Sobre `snprintf()`: [cplusplus.com](http://www.cplusplus.com/reference/cstdio/snprintf/)
- Sobre `memcpy()`: [cplusplus.com](http://www.cplusplus.com/reference/cstring/memcpy/)
- Más detalles sobre `strncpy()`: [geeksforgeeks.org](https://www.geeksforgeeks.org/strncpy-in-c-cpp/)