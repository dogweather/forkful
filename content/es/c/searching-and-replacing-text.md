---
title:                "C: Buscando y reemplazando texto"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

En este artículo, hablaremos sobre una tarea común en programación: la búsqueda y reemplazo de texto. Si eres programador o estudiante de programación, es muy probable que hayas tenido que realizar esta tarea en algún momento. Ya sea para corregir errores en tu código o para realizar cambios a gran escala, la búsqueda y reemplazo de texto es una herramienta esencial en el desarrollo de software.

## Por qué
La búsqueda y reemplazo de texto es útil para ahorrar tiempo y realizar cambios de manera eficiente. En lugar de tener que revisar manualmente cada línea de código, esta técnica te permite buscar y reemplazar automáticamente una cadena de texto por otra en todo el archivo. Esto es especialmente útil cuando se trabaja con archivos grandes o se tiene un gran número de cambios que hacer.

## Cómo hacerlo
La búsqueda y reemplazo de texto es una función que se encuentra disponible en la mayoría de los editores de texto y entornos de desarrollo integrados (IDE, por sus siglas en inglés). Sin embargo, también puedes escribir tu propia función de búsqueda y reemplazo en C.

A continuación, se presenta un ejemplo simple de cómo realizar una búsqueda y reemplazo en C:

```C
#include<stdio.h>

int main() {
    char string[] = "Hola Mundo";
    char buscar[] = "Mundo";
    char reemplazar[] = "amigos";

    char *resultado = strstr(string, buscar);

    if (resultado != NULL) {
        strncpy(resultado, reemplazar, strlen(reemplazar));
    }

    printf("%s", string);

    return 0;
}
```

En este ejemplo, tenemos una cadena de texto "Hola Mundo" y queremos reemplazar "Mundo" por "amigos". Utilizando la función `strstr()`, buscamos la primera aparición de "Mundo" dentro de la cadena y luego utilizamos`strncpy()` para reemplazarla por "amigos". Al final, imprimimos la cadena resultante "Hola amigos".

## Profundizando
Existen diversas funciones que pueden utilizarse para realizar una búsqueda y reemplazo en C, como `strstr()`, `strchr()`, entre otras. Cada una tiene sus propias características y es importante conocerlas para poder elegir la mejor opción para cada caso.

También es importante tener en cuenta que, al realizar una búsqueda y reemplazo, se debe prestar especial atención a la longitud de la cadena y los límites de memoria. En el ejemplo anterior, utilizamos `strncpy()` para asegurarnos de no sobrepasar el tamaño del arreglo de caracteres.

## Ver también
- [Tutorial de C para principiantes](https://www.programiz.com/c-programming)
- [Documentación de la librería estándar de C](https://www.cplusplus.com/reference/clibrary/)
- [Ejemplo de búsqueda y reemplazo en C++](https://www.programmingexamples.net/how-to-replace-all-occurrences-of-a-substring-in-a-string-with-another-string-in-c/)