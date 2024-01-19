---
title:                "Usando expresiones regulares"
html_title:           "Go: Usando expresiones regulares"
simple_title:         "Usando expresiones regulares"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

---
## ¿Qué & Por Qué?
Las expresiones regulares son una potente herramienta, usada en la programación para coincidir, encontrar o reemplazar patrones en los textos. Los programadores las utilizan para ahorrar tiempo y para manejar complejos patrones de búsqueda y reemplazo.

## Cómo hacerlo:
En C, utilizamos la biblioteca regex.h para trabajar con expresiones regulares. Les comparto un ejemplo de cómo detectar si un texto coincide con un patrón:

```C
#include <regex.h>
#include <stdio.h>

int main() {
    regex_t regex;    
    // Compila la expresión regular
    if (regcomp(&regex, "hola", 0)) {
        printf("No se pudo compilar\n");
        return 1;
    }

    // Revisa si el texto cumple con el patrón
    int res = regexec(&regex, "hola mundo", 0, NULL, 0);
    
    if (!res) printf("Coincide\n");
    else if (res == REG_NOMATCH) printf("No coincide\n");
    else printf("Error\n");

    regfree(&regex);
    return 0;
}
```

El output sería:

```
Coincide
```

## Deep Dive
Las expresiones regulares datan de la década de los 50, desarrolladas por el matemático Stephen Cole Kleene. No son exclusivas de C y en realidad se pueden encontrar en la mayoría de los lenguajes de programación.

Hay otras herramientas que también son capaces de manejar búsqueda y reemplazo de patrones, como 'strstr' en C pero no son tan poderosas como las expresiones regulares. 

La implementación de las expresiones regulares en C se maneja a través de la biblioteca regex.h, que nos permite utilizar funciones para compilar y aplicar las expresiones regulares.

## Ver también
- Documentación oficial de regex.h en C: https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html
- Herramienta en línea para probar expresiones regulares: https://regex101.com
- Curso intensivo de Regex para principiantes: https://www.codecademy.com/learn/learn-regex