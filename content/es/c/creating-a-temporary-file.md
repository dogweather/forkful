---
title:                "C: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

Existen diversas razones por las cuales se puede necesitar crear un archivo temporal durante la programación en C. Por ejemplo, puede ser útil para almacenar datos temporales que no sean necesarios después de la ejecución del programa, o para realizar operaciones que requieran un archivo temporal como espacio de almacenamiento.

## Cómo hacerlo

Crear un archivo temporal en C es un proceso relativamente sencillo. Primero, se debe incluir la biblioteca estándar de entrada y salida de archivos (stdio.h). Luego, se utiliza la función `fopen()` para crear un nuevo archivo temporal con el nombre y formato especificado. Por ejemplo:

```C
#include <stdio.h>
int main() {
    FILE *archivo;
    archivo = fopen("temp.csv", "w+");
    // código para escribir o leer en el archivo temporal
    fclose(archivo);
}
```

En el ejemplo anterior, se crea un archivo temporal llamado "temp.csv" con permisos de escritura y lectura (`w+`), y luego se cierra el archivo con la función `fclose()`. Es importante cerrar correctamente el archivo después de su uso para liberar la memoria y evitar errores en la ejecución del programa.

## Profundizando en la creación de archivos temporales

Además de los permisos de escritura y lectura, existen otros modos en los que se puede abrir un archivo temporal, como solo lectura (`r`), anexar al final del archivo existente (`a`) o sólo escritura (`w`).

También es posible especificar la ubicación del archivo temporal utilizando una ruta absoluta o relativa. Por defecto, el archivo se creará en el directorio de trabajo actual.

Es importante recordar que los archivos temporales se borran automáticamente cuando se cierran con la función `fclose()`. Sin embargo, si se desea eliminar el archivo antes de la finalización del programa, se puede utilizar la función `remove()` para hacerlo. Por ejemplo:

```C
#include <stdio.h>
int main() {
    FILE *archivo;
    archivo = fopen("temp.csv", "w+");
    // código para escribir o leer en el archivo temporal
    remove("temp.csv"); // elimina el archivo temporal
}
```

## Ver también

- [Función `fopen()` en C](https://www.lawebdelprogramador.com/foros/C-Visual-C/1025812-Documentacion-sobre-la-funcion-fopen.html)
- [Uso de archivos temporales en C](https://www.programacion.com.py/escritorio/c/uso-de-archivos-temporales-en-c)
- [Biblioteca estándar de entrada y salida de archivos en C](https://es.cppreference.com/w/c/io)

¡Esperamos que esta información te sea útil en tu experiencia de programación en C! Recuerda siempre cerrar correctamente tus archivos temporales para asegurar un buen funcionamiento del programa. ¡Happy coding!