---
title:                "C: Creando un archivo temporal"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por Qué

Los archivos temporales son una parte fundamental de la programación en C. Son archivos que se crean temporalmente durante la ejecución de un programa y se eliminan una vez que el programa termina de ejecutarse. Estos archivos son útiles para almacenar datos temporales o para realizar operaciones de procesamiento en segundo plano. En esta publicación, exploraremos cómo crear archivos temporales en C y por qué son importantes en el desarrollo de aplicaciones.

## Cómo hacerlo

Para crear un archivo temporal en C, debemos utilizar la función `tmpfile()` que se encuentra en la biblioteca estándar `<stdio.h>`. Esta función nos devuelve un puntero a una estructura `FILE` que representa el archivo temporal. A continuación se muestra un ejemplo de código que crea un archivo temporal, escribe una cadena en él y luego lo lee para mostrar su contenido:

```C
#include <stdio.h>

int main(void)
{
    // Crear un archivo temporal
    FILE *temp_file = tmpfile();
    
    // Comprobar si se pudo crear correctamente
    if (temp_file == NULL)
    {
        printf("Error creating temporary file!");
        return 1;
    }
    
    // Escribir una cadena en el archivo temporal
    fprintf(temp_file, "¡Hola, mundo!");
    
    // Volver al principio del archivo
    rewind(temp_file);
    
    // Leer y mostrar el contenido del archivo
    char str[15];
    fscanf(temp_file, "%s", str);
    printf("Temp file content: %s", str);
    
    // Cerrar y eliminar el archivo temporal
    fclose(temp_file);
    
    return 0;
}
```

La salida de este programa sería:

```
Temp file content: ¡Hola, mundo!
```

## Inmersión Profunda

Cuando se crea un archivo temporal en C, en realidad se crea un archivo en la carpeta temporal del sistema operativo. Esto se puede verificar usando la función `tmpnam()` que nos devuelve un nombre de archivo único para un archivo temporal. Además, se puede especificar una ruta personalizada para el archivo temporal utilizando la función `tmpfile()`. Es importante tener en cuenta que estos archivos temporales son eliminados automáticamente una vez que el programa termina su ejecución.

También es importante mencionar que, aunque los archivos temporales son útiles para almacenar datos temporales, no se recomienda su uso para almacenar datos sensibles ya que pueden ser accesibles por otros programas o usuarios en el sistema.

## Ver También

- Documentación de la función `tmpfile()`: https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm
- Ejemplo de código para crear y utilizar un archivo temporal en C: https://www.geeksforgeeks.org/create-temporary-file-fopen-in-c/
- Explicación sobre archivos temporales en C: https://www.ibm.com/support/knowledgecenter/ssw_ibm_i_74/apis/tmpfile.htm