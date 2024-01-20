---
title:                "Capitalizando una cadena de texto"
html_title:           "C: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Capitalizar una cadena significa convertir la primera letra de cada palabra en mayúsculas. Los programadores a menudo lo hacen para mejorar la legibilidad y estandarizar los datos de entrada.

## Cómo se hace:

Aquí te muestro un ejemplo de cómo capitalizar una cadena en C:

```C
#include <stdio.h>
#include <ctype.h>

void capitalizarCadena(char *str) {
    while (*str != '\0') {
        *str = toupper((unsigned char) *str);
        str++;
    }
}

int main() {
    char str[] = "hola mundo!";
    capitalizarCadena(str);
    printf("%s\n", str);  // Salida: "HOLA MUNDO!"
    return 0;
}
```

## Un Vistazo Más Profundo

La capitalización de cadenas tiene sus raíces en la mecanografía y los lenguajes de programación la han adoptado desde entonces. En C, la capitalización es simple pero hay variaciones en la implementación dependiendo de tus necesidades. Podrías usar `toupper`, que es la forma utilizada en el ejemplo anterior, o podrías usar `towupper` en el caso de las cadenas ancha que soporta la internacionalización del lenguaje. Sin embargo, recuerda que estas funciones solo convierten las letras a mayúsculas. Si quieres convertir cada primera letra de una palabra, tendrías que implementar lógica adicional para detectar los espacios.

## Ver También:

Aquí tienes algunos recursos útiles para entender más profundamente este concepto:

- "toupper", la función estándar de C - [Documentación](https://www.cplusplus.com/reference/cctype/toupper/)
- "towupper", para cadenas anchas - [Documentación](https://en.cppreference.com/w/c/string/wide/towupper)