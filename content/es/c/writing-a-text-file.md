---
title:    "C: Escribiendo un archivo de texto"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez ha necesitado guardar información en un archivo de texto en su programa de C? ¡Entonces ha venido al lugar correcto! En este artículo, le mostraremos cómo escribir un archivo de texto en C y profundizaremos en por qué es útil hacerlo.

## Cómo hacerlo

Para escribir un archivo de texto en C, necesitaremos utilizar la biblioteca estándar de C <stdio.h> y sus funciones `fopen()` y `fprintf()`. Primero, abrimos el archivo con la función `fopen()` indicando el nombre del archivo y el modo de apertura, que puede ser "w" para escribir en el archivo. Luego, utilizamos la función `fprintf()` para escribir el contenido que deseamos en el archivo, especificando el archivo abierto y el texto que queremos escribir. Finalmente, cerramos el archivo con la función `fclose()`.

Veamos un ejemplo práctico:
```C
#include <stdio.h>

int main() 
{
    FILE *archivo = fopen("mi_archivo.txt", "w");
    
    if (archivo == NULL) 
    {
        printf("Error abriendo el archivo.");
        return 1;
    }
    
    fprintf(archivo, "¡Hola, mundo!");
    fclose(archivo);
    
    return 0;
}
```
La salida de este código será un archivo de texto llamado "mi_archivo.txt" con la siguiente línea escrita: ¡Hola, mundo!

## Profundizando

Ahora que sabemos cómo escribir un archivo de texto en C, es importante entender por qué es útil hacerlo. Al guardar información en un archivo de texto, podemos crear un registro de datos que podemos acceder y leer en cualquier momento. Esto es especialmente útil en programas que manejan grandes cantidades de información o requieren datos persistentes entre sesiones.

También podemos utilizar la escritura de archivos de texto en conjunto con la lectura de archivos para crear bases de datos simples o almacenar configuraciones de programas.

## Ver también

- [Cómo leer un archivo de texto en C] (link a un artículo sobre cómo leer archivos de texto en C)
- [Introducción a la biblioteca estándar de C] (link a un artículo sobre la biblioteca estándar de C)