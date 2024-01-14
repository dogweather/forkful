---
title:    "C: Extrayendo subcadenas."
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas es una habilidad esencial para aquellos que trabajan con cadenas de texto en lenguaje C. Puede ser necesario para realizar operaciones como buscar y reemplazar palabras dentro de una cadena o para separar una cadena en secciones más pequeñas. Además, extraer subcadenas puede mejorar la eficiencia del código y facilitar la manipulación de datos.

## Cómo hacerlo

Para extraer subcadenas en C, primero debemos tener una cadena de texto existente y determinar la posición de inicio y fin de la subcadena que deseamos extraer. Podemos hacer esto utilizando la función `strlcpy()` y especificando la posición de inicio y el número de caracteres que queremos copiar.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char texto[] = "Este es un ejemplo de una cadena de texto";
    char subcadena[20];
    int posicion_inicio = 8; // especificamos la posición de inicio
    int numero_caracteres = 6; // especificamos el número de caracteres a copiar

    // utilizamos la función strlcpy() para extraer la subcadena
    strlcpy(subcadena, texto + posicion_inicio, numero_caracteres + 1);
    printf("La subcadena es: %s", subcadena);

    return 0;
}
```

El resultado de este código sería: 
```
La subcadena es: un ej
```

También podemos utilizar la función `strncpy()` para extraer subcadenas en C. Esta función es similar a `strlcpy()`, pero requiere que especifiquemos la longitud máxima de la subcadena en lugar del número de caracteres que deseamos copiar.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char texto[] = "Este es un ejemplo de una cadena de texto";
    char subcadena[20];
    int posicion_inicio = 21; // especificamos la posición de inicio
    int longitud_maxima = 11; // especificamos la longitud máxima de la subcadena

    // utilizamos la función strncpy() para extraer la subcadena
    strncpy(subcadena, texto + posicion_inicio, longitud_maxima);
    printf("La subcadena es: %s", subcadena);

    return 0;
}
```

El resultado de este código sería: 
```
La subcadena es: cadena de t
```

## Profundizando

En la programación en C, no siempre es necesario utilizar las funciones `strlcpy()` o `strncpy()` para extraer subcadenas. Podemos lograr lo mismo utilizando punteros para señalar a una posición específica en una cadena de texto y luego manipular el puntero para obtener la subcadena deseada.

Por ejemplo, consideremos el siguiente código que utiliza punteros para extraer una subcadena en lugar de utilizar las funciones `strlcpy()` o `strncpy()`:

```C
#include <stdio.h>

int main() {
    char texto[] = "Este es un ejemplo de una cadena de texto";
    char subcadena[20];
    char *puntero = texto + 16; // señala a la posición de inicio de la subcadena

    // utilizando un bucle for, copiamos cada caracter de la subcadena en nuestro arreglo
    for (int i = 0; i < 6; i++) {
        subcadena[i] = *puntero;
        puntero++; // avanzamos el puntero a la siguiente posición
    }

    printf("La subcadena es: %s", subcadena);

    return 0;
}
```

El resultado sería el mismo que en los ejemplos anteriores: 
```
La subcadena es: una ca
```

## Ver También
- [La función `strlcpy()` en la documentación de C](https://www.mercurial-scm.org/repo/hosted-docfilesscmintro_20120525/1f1ee630b953/summary-scm/intro-docs-english/html/c_stdlib-string.html)
- [La función `strncpy()` en la documentación de C](https://www.gnu.org/software/libc/manual/html_node/Copying-and-Concatenation.html)
- [Manipulación de cadenas en C](https://www.programiz.com/c-programming/c-strings)