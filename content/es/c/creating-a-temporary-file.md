---
title:    "C: Creando un archivo temporal"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por qué

Crear un archivo temporal en un programa de C puede parecer innecesario, pero puede ser muy útil en ciertas situaciones. Por ejemplo, puede utilizar un archivo temporal para almacenar información que sólo necesita durante la ejecución del programa y que no es necesaria una vez que el programa haya finalizado. Además, los archivos temporales pueden ser utilizados para realizar tareas de mantenimiento en el sistema, como copiar o mover archivos.

## Como Hacerlo

Para crear un archivo temporal en un programa de C, utilizamos la función `tmpfile()`. Esta función crea un archivo vacío en la ruta temporal del sistema y devuelve un puntero al archivo en el que se puede escribir y leer datos. Veamos un ejemplo de cómo utilizar esta función:

```
#include <stdio.h>

int main()
{
    FILE *archivo_temp;

    archivo_temp = tmpfile();

    if(archivo_temp == NULL)
    {
        printf("Error al crear el archivo temporal.");
        return 1;
    }

    fprintf(archivo_temp, "Este es un ejemplo de archivo temporal.\n");

    fclose(archivo_temp);

    printf("Archivo temporal creado exitosamente.");

    return 0;
}
```

En este ejemplo, utilizamos `tmpfile()` para crear un archivo temporal y luego escribimos una línea de texto en él utilizando la función `fprintf()`. Una vez que hemos terminado de utilizar el archivo, lo cerramos con `fclose()` y nuestro programa termina sin dejar residuos del archivo temporal en el sistema.

## Profundizando

Además de `tmpfile()`, también existen otras funciones para crear archivos temporales en C, como `tmpnam()` y `mkstemp()`. Estas funciones ofrecen mayor control sobre la ubicación y el nombre del archivo temporal que se creará.

Es importante tener en cuenta que los archivos temporales suelen ser eliminados automáticamente por el sistema operativo cuando el programa termina su ejecución, pero en algunos casos puede que sea necesario eliminarlos manualmente utilizando la función `remove()`.

## Mira también

* [Función tmpfile() en la documentación de C](https://www.learncpp.com/cpp-tutorial/186-creating-a-temporary-file-with-tmpfile/)
* [Ejemplos de uso de archivos temporales en C](https://www.programmingwithwolfgang.com/c-programming-tutorial-create-temporary-files/)