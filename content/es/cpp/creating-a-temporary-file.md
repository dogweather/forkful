---
title:                "Creando un archivo temporal"
html_title:           "C++: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

Muchas veces, en la programación, necesitas crear un archivo temporal para almacenar datos de manera temporal. Puedes usar esta técnica cuando necesitas hacer cálculos complejos, realizar pruebas o simplemente para evitar sobrecargar tu disco duro con archivos innecesarios. Ahora, vamos a aprender cómo crear un archivo temporal en C++.

## Cómo hacerlo

Para crear un archivo temporal en C++, necesitamos incluir la librería `<fstream>` que nos permitirá trabajar con archivos. Luego, usamos la función `tmpnam()` que crea un nombre único para el archivo temporal. A continuación, utilizamos la función `fopen()` para crear y abrir el archivo temporal en modo de escritura ("w"). Finalmente, podemos escribir en el archivo temporal usando la función `fprintf()` y cerrarlo con `fclose()` cuando hayamos terminado.

```C++
#include <fstream>

int main() {
    // Crear un nombre único para el archivo temporal
    char nombreArchivo[L_tmpnam];
    tmpnam(nombreArchivo);
    
    // Crear y abrir el archivo temporal
    FILE* archivo = fopen(nombreArchivo, "w");
    
    // Escribir en el archivo temporal
    fprintf(archivo, "¡Hola mundo!\n");
    
    // Cerrar el archivo
    fclose(archivo);
    return 0;
}
```

### Resultado

Después de ejecutar este código, puedes revisar tu disco duro y encontrarás un nuevo archivo temporal con un nombre aleatorio (por ejemplo, `tmpUVGABF`) que contiene el mensaje "¡Hola mundo!".

## Profundizando

Si deseas tener un mayor control sobre la creación del archivo temporal, puedes usar la función `mkstemp()` en lugar de `tmpnam()`. Esta función crea un archivo temporal en un directorio determinado y te permite especificar un prefijo para el nombre del archivo temporal. Además, también hay funciones para manejar el borrado automático del archivo temporal después de su uso.

## Véase también

- [Documentación oficial de C++ para crear archivos temporales](https://en.cppreference.com/w/c/io/fopen)
- [Aprende C++ con W3Schools](http://www.w3schools.com/cpp/)
- [Tutorial de C++ para principiantes](https://www.programiz.com/cpp-programming)