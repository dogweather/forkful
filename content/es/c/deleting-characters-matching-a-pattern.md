---
title:                "Eliminando caracteres que coinciden con un patrón"
date:                  2024-01-20T17:41:37.730353-07:00
model:                 gpt-4-1106-preview
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Eliminar caracteres que coinciden con un patrón es básicamente buscar y destruir ciertas secuencias en un texto. Los programadores hacen esto para limpiar datos, validar entradas o procesar texto de manera más eficiente.

## Cómo hacerlo:
Usaremos la función `strremove()` que añadieron en C23 para hacer este trabajo de forma simple y directa.

```c
#include <stdio.h>
#include <string.h>

void strremove(char *str, const char *sub) {
    char *p, *q, *r;
    if (*sub && (q = r = strstr(str, sub)) != NULL) {
        size_t len = strlen(sub);
        while ((r = strstr(p = r + len, sub)) != NULL) {
            while (p < r)
                *q++ = *p++;
        }
        while ((*q++ = *p++));
    }
}

int main() {
    char text[] = "Hola Mundo! Programamos en C sin rodeos.";
    strremove(text, "Mundo!");
    printf("Texto limpio: %s\n", text);
    return 0;
}

```

Salida esperada:
```
Texto limpio: Hola  Programamos en C sin rodeos.
```

Notarás que la función `strremove()` localiza la subcadena y la elimina, y luego mueve el resto de la cadena para llenar el hueco.

## Inmersión Profunda
Eliminar caracteres que coincidan con un patrón no siempre ha sido tan sencillo en C. Históricamente, habrías usado bucles y aritmética de punteros. La función `strremove()` es una adición muy reciente en C23. Alternativas como `strchr()`, `strstr()`, o el uso de expresiones regulares con `regex.h` existen pero son más complejas. La implementación mostrada arriba, aunque hecha a mano, es bastante directa: encuentra la subcadena, sobrescribe y avanza.

## Ver También
Para expandir tus conocimientos, consulta la documentación oficial de funciones de manipulación de cadenas en C:
- [C String Library](https://en.cppreference.com/w/c/string/byte)

Si tienes curiosidad por las soluciones de expresiones regulares en C, mira: