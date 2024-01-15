---
title:                "Creando un archivo temporal"
html_title:           "C: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

¿Alguna vez has tenido que crear un archivo temporal en tu programa C? Para muchas personas puede parecer una tarea innecesaria, pero hay varias razones por las que alguien querría crear un archivo temporal. Una de las principales razones es para almacenar datos temporales que no son necesarios una vez que el programa ha terminado de ejecutarse. También puede ser útil para guardar archivos descargados de internet o guardar información antes de realizar cambios en un archivo existente.

## How To

Para crear un archivo temporal en C, utilizaremos la función `tmpfile()`. Esta función nos permitirá crear un archivo temporal y guardar su flujo de datos en una variable. Veamos un ejemplo:

```C
#include <stdio.h>
#include <stdlib.h>

int main(){
    FILE *temp_file;
    temp_file = tmpfile();
    if(temp_file == NULL){
        printf("No se pudo crear el archivo temporal.");
        exit(1);
    }
    // Aquí podemos escribir o leer datos en el archivo temporal.
    // ...
    fclose(temp_file); // Cerramos el archivo al finalizar.
    return 0;
}
```

En este ejemplo, primero incluimos las cabeceras `stdio.h` y `stdlib.h` que nos permitirán utilizar las funciones `printf()` y `exit()`. Luego, declaramos una variable `temp_file` de tipo `FILE` que guardará el flujo de datos del archivo temporal. A continuación, llamamos a la función `tmpfile()` y asignamos su resultado a la variable `temp_file`. Si esta función regresa `NULL`, quiere decir que no se pudo crear el archivo temporal y por lo tanto, imprimimos un mensaje de error y terminamos el programa. Si todo sale bien, podemos utilizar el archivo temporal para escribir o leer datos y luego cerrarlo utilizando la función `fclose()`.

Podemos imprimir el contenido del archivo temporal utilizando la función `fprintf()` y pasarle como parámetro el archivo y los datos a imprimir, o también podemos leer el contenido utilizando la función `fscanf()`. Al finalizar, es importante cerrar el archivo utilizando la función `fclose()` para no ocupar memoria innecesariamente.

## Deep Dive

La función `tmpfile()` es útil cuando solo necesitamos un archivo temporal para almacenar datos durante la ejecución de un programa. Sin embargo, también podemos utilizar la función `fopen()` para crear un archivo temporal y tener más control sobre su nombre y ubicación en el sistema de archivos.

Por ejemplo, podemos utilizar la función `tmpnam()` para generar un nombre aleatorio para nuestro archivo temporal y luego usarlo en la función `fopen()` para crear el archivo en una ubicación específica. También podemos utilizar la función `mkstemp()` para crear un archivo temporal con un nombre único y devolvernos un descriptor de archivo que podemos utilizar para escribir o leer datos.

Otro aspecto importante a tener en cuenta cuando se trata de crear archivos temporales es su eliminación. Es importante que una vez que ya no se necesite el archivo temporal, este sea eliminado para no ocupar espacio innecesariamente. Podemos utilizar la función `remove()` para eliminar un archivo del sistema de archivos. Sin embargo, si estamos utilizando la función `mkstemp()`, debemos asegurarnos de que el archivo temporal sea eliminado después de ser utilizado.

## See Also

Si estás interesado en aprender más sobre el manejo de archivos en C, aquí te dejo algunos enlaces útiles:

- [Manejo de archivos en C - Tutorialspoint](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [Creación de archivos temporales en C - GeeksforGeeks](https://www.geeksforgeeks.org/creating-temporary-files-in-c/)
- [Cómo eliminar un archivo en C - Stack Overflow](https://stackoverflow.com/questions/14002954/how-to-delete-a-file-using-c)