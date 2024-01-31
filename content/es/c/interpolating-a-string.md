---
title:                "Interpolación de cadenas de texto"
date:                  2024-01-20T17:50:17.217490-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolación de cadenas de texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
La interpolación de cadenas inserta valores de variables en medio de texto estático, haciendo que su manipulación sea más intuitiva. Los programadores la usan para crear mensajes dinámicos sin separar demasiado la lógica del texto.

## Cómo hacerlo:
```C
#include <stdio.h>

int main() {
    char name[] = "Mundo";
    int age = 30;

    // Utilizando snprintf para la interpolación
    char message[50];
    snprintf(message, sizeof(message), "Hola, %s! Tienes %d años.", name, age);

    printf("%s\n", message);
    // Output: Hola, Mundo! Tienes 30 años.
    
    return 0;
}
```

## Buceo Profundo:
La interpolación de cadenas no se incorpora nativamente en C como en otros lenguajes más modernos; se realiza a menudo con `printf` o sus funciones derivadas como `sprintf` y `snprintf`. Históricamente, los lenguajes de programación no siempre ofrecieron herramientas para la interpolación directa, forzando a los programadores a concatenar cadenas manualmente. Alternativas incluyen la construcción de cadenas con `strcat` y `strcpy`, pero `snprintf` es preferido por su seguridad de buffer. Cuidado con `sprintf`, ya que no verifica el tamaño del buffer y puede causar desbordamiento de búfer.

## Ver También:
- Documentación de `printf`: http://www.cplusplus.com/reference/cstdio/printf/
- Guía sobre `snprintf`: https://en.cppreference.com/w/c/io/fprintf
