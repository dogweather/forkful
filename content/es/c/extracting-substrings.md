---
title:    "C: Extrayendo subcadenas"
keywords: ["C"]
---

{{< edit_this_page >}}

# ¿Por qué extraer subcadenas en programación?

La extracción de subcadenas es una operación común en programación cuando se trabaja con cadenas de texto. Esta técnica se utiliza para obtener una parte específica de una cadena más grande, ya sea para realizar alguna operación o simplemente para presentar información de manera clara y concisa. A continuación, veremos cómo extraer subcadenas en lenguaje C.

## Cómo hacerlo:

Para extraer una subcadena en C, necesitamos dos cosas: la cadena original y la posición inicial y final de la subcadena que deseamos obtener. A continuación, se muestra un ejemplo de código utilizando la función integrada `strstr()` para encontrar la posición de una subcadena específica dentro de una cadena más grande.

```
#include<stdio.h>

int main(){
    char cadena[] = "¡Hola mundo!";
    char subcadena[] = "mundo";
    char *puntero;

    // Encuentra la posición de la subcadena en la cadena original
    puntero = strstr(cadena, subcadena);

    // Imprime la subcadena
    printf("Subcadena: %s\n", puntero);

    return 0;
}
```

**Salida:**
```
Subcadena: mundo!
```

En este ejemplo, utilizamos la función `strstr()` para encontrar la posición de la subcadena "mundo" dentro de la cadena "¡Hola mundo!". Luego, imprimimos esa subcadena utilizando un puntero. Esta es una forma sencilla de extraer una subcadena en C.

## Un vistazo más profundo:

En la sección anterior, solo vimos un ejemplo básico de cómo extraer subcadenas en C. Sin embargo, hay más formas de hacerlo y también es importante entender cómo funciona realmente esta operación.

Una forma más común de extraer subcadenas es utilizando la función `strncpy()`, que copia una cantidad especificada de caracteres de una cadena a otra.

```
#include<stdio.h>
#include<string.h>

int main(){
    char cadena[] = "¡Hola mundo!";
    char subcadena[6];

    // Copia los primeros 5 caracteres de la cadena original a la subcadena
    strncpy(subcadena, cadena, 5);

    // Agrega un caracter nulo al final de la subcadena
    subcadena[5] = '\0';

    // Imprime la subcadena
    printf("Subcadena: %s\n", subcadena);

    return 0;
}
```

**Salida:**
```
Subcadena: ¡Hola
```

En este ejemplo, utilizamos la función `strncpy()` para copiar los primeros 5 caracteres de la cadena original a una nueva subcadena. Luego, agregamos un caracter nulo al final de la subcadena para asegurarnos de que se imprima correctamente.

También es posible extraer subcadenas utilizando un bucle `for` y accediendo a cada carácter de la cadena original utilizando su índice. Sin embargo, esta técnica puede ser más tediosa y propensa a errores.

## Ver también:

- [Documentación de la función `strstr()` en C](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)
- [Documentación de la función `strncpy()` en C](https://www.tutorialspoint.com/c_standard_library/c_function_strncpy.htm)

# ¡Es hora de empezar a extraer subcadenas en C!

Ahora que tienes una mejor comprensión de cómo extraer subcadenas en C, puedes aplicar esta técnica en tus propios proyectos y mejorar la manipulación de cadenas de texto en tus programas. Experimenta con diferentes funciones y métodos para encontrar el que mejor se adapte a tus necesidades. ¡Buena suerte!