---
title:                "Interpolando una cadena de texto"
html_title:           "Haskell: Interpolando una cadena de texto"
simple_title:         "Interpolando una cadena de texto"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

---

## ¿Qué y Por qué?
La interpolación de strings permite combinar datos de variables con cadenas de texto. Los programadores lo hacen para manejar y presentar de manera fácil y eficiente la información contenida en las variables.

## Cómo hacerlo:
A continuación, mostraremos unos ejemplos de cómo se puede realizar la interpolación de strings en lenguaje C.

```C
#include <stdio.h>

int main() {
    // Declaramos las variables
    int count = 5;
    char name[] = "Carlos";
    
    // Utilizamos la función printf para la interpolación
    printf("Hola, %s. Tienes %d mensajes nuevos.\n", name, count);

    return 0;
}
```
Salida: 
```C 
Hola, Carlos. Tienes 5 mensajes nuevos.
``` 

## Detalles Adicionales:
La interpolación de strings tal y como la conocemos hoy en día, tiene sus raíces en los lenguajes ALGOL 68 y C, en donde se introdujo la función printf() y suzo enumerable de especificadores de formato. Sin embargo, en lenguaje C, la interpolación no es tan directa como en otros lenguajes de alto nivel. En C, debemos usar las funciones `printf()` o `sprintf()` con indicadores de formato correspondientes.

Un enfoque alternativo en C sería el uso de la función `sprintf()`, que puede ser útil cuando quieras almacenar el resultado en un buffer o una cadena. Sin embargo, debes ser consciente del tamaño de las cadenas, buffer overflow es una amenaza de seguridad importante en C.

```C
char buffer[50];
int n = sprintf(buffer, "Hola, %s. Tienes %d mensajes nuevos.", name, count);
```

## Ver También:
- Documentación Oficial de la función printf: www.cplusplus.com/reference/cstdio/printf
- Guía Práctica de la función sprintf: www.geekhideout.com/printf
- Inteligencia de cadenas con sprintf: www.ibm.com/developerworks/library/l-sprntf
- Precauciones de seguridad para la manipulación de cadenas en C: owasp.org/www-community/Buffer_Overflow

---