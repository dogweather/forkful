---
title:                "Extracción de subcadenas"
date:                  2024-01-20T17:45:02.032134-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extracción de subcadenas"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Extraer subcadenas es sacar partes específicas de una cadena de texto. Es útil para procesar información, validar datos o simplemente para obtener lo que necesitas de un montón de caracteres.

## How to:
Aquí tienes un fragmento de cómo extraer una subcadena en C usando la función `strncpy`:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Hola, programador!";
    char sub[10];

    // Copia 9 caracteres empezando desde la posición 0
    strncpy(sub, str, 9);
    sub[9] = '\0'; // Asegúrate de terminar con NULL

    printf("Subcadena: %s\n", sub);

    return 0;
}
```

Salida:
```
Subcadena: Hola, pro
```

## Deep Dive
Extraer subcadenas es un concepto antiguo en la programación. En C, aunque no hay una función integrada que haga esto directamente, `strncpy` es comúnmente usada. Las alternativas podrían ser `sscanf` o manipulación manual de índices. La implementación precisa de `strncpy` es fundamental para evitar errores como faltas de terminación `NULL` o desbordamientos del buffer. 

## See Also
Aquí tienes algunos recursos para seguir aprendiendo:

- Documentación de `strncpy`: https://www.cplusplus.com/reference/cstring/strncpy/
- Buenas prácticas con strings en C: http://sekrit.de/webdocs/c/beginners-guide-away-from-scanf.html
- Fundamentos de `sscanf`: https://en.cppreference.com/w/c/io/fscanf
