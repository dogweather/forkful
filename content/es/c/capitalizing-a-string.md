---
title:                "Capitalizando una cadena de texto"
date:                  2024-01-19
html_title:           "Arduino: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizar una cadena significa convertir todas las letras a mayúsculas. Los programadores lo hacen para normalizar los datos de entrada o mejorar la legibilidad del texto.

## How to:
```C
#include <stdio.h>
#include <ctype.h>

void capitalizarCadena(char *str) {
    while (*str) {
        *str = toupper((unsigned char) *str);
        str++;
    }
}

int main() {
    char miCadena[] = "¡Hola, mundo!";
    capitalizarCadena(miCadena);
    printf("Cadena Capitalizada: %s\n", miCadena);
    return 0;
}
```
Salida:
```
Cadena Capitalizada: ¡HOLA, MUNDO!
```

## Deep Dive
Capitalizar cadenas es un proceso antiguo en la programación, utilizado en varios lenguajes desde los primeros días de la informática. En C, la función `toupper` de la biblioteca estándar es una manera sencilla de convertir un carácter individual a mayúscula. Para capitalizar una cadena completa, recórrela carácter por carácter, aplicando `toupper` a cada uno.

Existen alternativas como usar funciones de bibliotecas de terceros o escribir tu propios algoritmos adaptados a un caso de uso particular, pero `toupper` es suficiente para la mayoría de las situaciones y está ampliamente disponible.

Detalles a considerar al implementar la capitalización de cadenas incluyen:
1. Trabajar cuidadosamente con caracteres que no son ASCII, ya que `toupper` puede no manejarlos correctamente sin configuraciones adicionales.
2. Asegurarte de que la cadena de entrada está terminada correctamente con un carácter nulo para evitar problemas de memoria.

## See Also
- Documentación de la función `toupper`: https://en.cppreference.com/w/c/string/byte/toupper
- Recomendaciones de estilo de codificación en C: https://www.gnu.org/prep/standards/standards.html
- Unicode y programación en C: https://www.unicode.org/reports/tr35/tr35-6.html
