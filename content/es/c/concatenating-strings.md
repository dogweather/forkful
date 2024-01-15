---
title:                "Concatenando cadenas"
html_title:           "C: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Por qué concatenar cadenas de texto en C?

Una de las tareas más comunes en programación es la manipulación de cadenas de texto. Ya sea para mostrar un mensaje al usuario, para realizar operaciones matemáticas con números representados como texto, o para almacenar y analizar información, es necesario trabajar con cadenas de texto de alguna forma u otra. Y una de las formas más eficientes de hacerlo en el lenguaje de programación C es mediante la concatenación de cadenas.

## Cómo hacerlo

La concatenación de cadenas en C se logra mediante el uso de la función "strcat" que se encuentra en la biblioteca de cadenas "string.h". Esta función toma dos cadenas como parámetros y concatena la segunda cadena al final de la primera. Veamos un ejemplo:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char saludo[15] = "¡Hola ";
    char nombre[10] = "amigo!";
    strcat(saludo, nombre);
    printf("%s", saludo);
    return 0;
}
```

En este ejemplo, la función "strcat" concatena la palabra "amigo!" al final de la cadena "¡Hola ", resultando en la cadena "¡Hola amigo!". Una vez que se ha realizado la concatenación, la cadena original se modifica, por lo que es importante asegurarse de tener suficiente espacio en la cadena original para la adición de la segunda cadena.

## Profundizando en la concatenación de cadenas

Otra forma de concatenar cadenas en C es mediante el uso de la función "sprintf". Esta función también se encuentra en la biblioteca "string.h" y permite formatear cadenas de texto con distintos tipos de datos. Veamos un ejemplo:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char frase[50];
    int numero = 10;
    sprintf(frase, "El número es %d", numero);
    printf("%s", frase);
    return 0;
}
```

En este ejemplo, la función "sprintf" formatea la variable "numero" como un entero y la concatena a la cadena "El número es ". Luego, la cadena resultante se guarda en la variable "frase", que puede ser impresa con la función "printf". Esta forma de concatenación permite una mayor flexibilidad en cuanto al formato de los datos que deseamos agregar a la cadena.

## Ver también

- Documentación oficial de la función "strcat": https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm
- Documentación oficial de la función "sprintf": https://www.tutorialspoint.com/c_standard_library/c_function_sprintf.htm