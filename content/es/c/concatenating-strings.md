---
title:                "Concatenando cadenas de texto"
html_title:           "Arduino: Concatenando cadenas de texto"
simple_title:         "Concatenando cadenas de texto"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concatenando cadenas en C (Versión Actual)

## ¿Qué y Por qué?

La concatenación de cadenas es el proceso de combinar dos o más cadenas en una. Los programadores lo hacen para manipular o juntar datos textuales.

## ¿Cómo hacerlo?

Aquí hay un ejemplo básico de cómo concatenar cadenas.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char s1[20] = "Hola, ";
    char s2[] = "Mundo.";
    strcat(s1, s2);

    printf("%s\n", s1); 
    return 0;
}
```
La salida será:

`Hola, Mundo.`

En este ejemplo, hemos utilizado la función `strcat()` de la biblioteca `string.h` para concatenar `s1` y `s2`.

## Mirada en Profundidad 

En el contexto histórico, la concatenación de cadenas no era un proceso sencillo en las primeras versiones de C. Sin embargo, a medida que el lenguaje evolucionó, la biblioteca `string.h` se introdujo con funciones como `strcat()`, facilitando el proceso significativamente.

Hay otras formas de concatenar cadenas en C, como el uso de punteros o `sprintf()`. Cada método tiene sus propias ventajas y desventajas. Dependiendo de las necesidades del problema, los programadores pueden preferir un método sobre otro.

La implementación de la concatenación de cadenas en C reserva espacio para la cadena de destino que sea lo suficientemente amplia para contener la cadena de origen y la cadena de destino concatenada.

## Ver También 

Para más información sobre la concatenación de cadenas en C y funciones relacionadas, puedes visitar los siguientes enlaces:

- [Documentación oficial de GCC (GNU Compiler Collection)](https://gcc.gnu.org/onlinedocs/gcc-4.4.1/gcc/String-Handling.html)
- [Tutorial de TutorialsPoint sobre la concatenación de cadenas en C](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [Información profunda sobre `strcat()`](https://www.cplusplus.com/reference/cstring/strcat/) 
- [Información sobre el manejo de cadenas en C](http://www.learn-c.org/en/String_Manipulation)