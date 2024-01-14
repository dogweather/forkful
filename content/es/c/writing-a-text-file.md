---
title:                "C: Redacción de un archivo de texto"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Escribir un archivo de texto es una tarea sencilla y fundamental en la programación en C. Los archivos de texto son utilizados para guardar información de manera permanente y permiten que los programas puedan acceder y manipular estos datos. Esto es esencial ya sea para almacenar configuraciones, registros o cualquier otra información que deba persistir después de que el programa finalice su ejecución.

## Cómo

La escritura de un archivo de texto en C se puede dividir en tres pasos fundamentales: abrir, escribir y cerrar el archivo. Primero, necesitamos declarar un puntero a tipo FILE, el cual actuará como nuestro acceso al archivo. Luego, con la función `fopen()` podemos abrir el archivo en el modo deseado, ya sea solo lectura, solo escritura o lectura/escritura.

```C
#include <stdio.h>

int main()
{
    FILE *archivo;
    archivo = fopen("ejemplo.txt", "w");
    /* código para escribir en el archivo */
    fclose(archivo);
    return 0;
}
```
Una vez que el archivo está abierto, podemos utilizar la función `fprintf()` para escribir en él. Esta función funciona de manera similar a `printf()`, pero en vez de imprimir en la pantalla, imprime en el archivo.

```C
fprintf(archivo, "Este es un ejemplo de texto que será escrito en el archivo.");
```

Finalmente, cuando ya hemos terminado de escribir en el archivo, debemos cerrarlo utilizando la función `fclose()`. Esto asegura que la información se guarde y el archivo se cierre adecuadamente.

## Profundizando

Además de las funciones mencionadas anteriormente, existen otras funciones y opciones que pueden ser útiles al momento de escribir un archivo de texto en C. Por ejemplo, la función `fputc()` permite escribir un solo caracter en el archivo y `fputs()` permite escribir una cadena de caracteres. También es posible utilizar el modo "append" al abrir un archivo (utilizando "a" en vez de "w") para añadir información al final del archivo en lugar de sobrescribirlo completamente.

Ten en cuenta que al escribir un archivo en C, es importante asegurarse de que siempre se cierre de manera adecuada. De lo contrario, podemos perder información o incluso dañar el archivo.

## Ver también

- [Tutorial de archivos en C](https://www.programiz.com/c-programming/c-file-input-output)
- [Documentación oficial de fopen()](https://www.cplusplus.com/reference/cstdio/fopen/)
- [Declarando y utilizando archivos en C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)