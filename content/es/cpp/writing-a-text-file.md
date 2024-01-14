---
title:                "C++: Escribiendo un archivo de texto"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué escribir un archivo de texto en C++

Escribir un archivo de texto en C++ es una tarea común que se puede utilizar para diversas aplicaciones. Esto incluye guardar datos de una aplicación, crear archivos de configuración o incluso generar informes.

## Cómo escribir un archivo de texto en C++

Escribir un archivo de texto en C++ es un proceso sencillo que requiere de algunas técnicas básicas de programación. Primero, es necesario incluir la librería `fstream` para manejar archivos. Luego, se debe abrir el archivo deseado con la función `std::ofstream`, especificando el nombre y el modo de apertura (escritura en este caso). A continuación, se pueden escribir datos en el archivo utilizando el operador de flujo `<<`. Finalmente, se debe cerrar el archivo con la función `close()`, asegurándose de que todos los datos hayan sido escritos correctamente.

```C++
#include <iostream>
#include <fstream>

int main() {
    std::ofstream archivo("ejemplo.txt"); // Abre el archivo para escritura
    archivo << "Este es un ejemplo de texto que será escrito en un archivo." << std::endl; // Escribe datos en el archivo
    archivo.close(); // Cierra el archivo
    return 0;
}
```

El archivo resultante se verá así:

`ejemplo.txt`
```
Este es un ejemplo de texto que será escrito en un archivo.
```

## Profundizando en la escritura de archivos de texto en C++

Además de la técnica básica mencionada anteriormente, existen otras formas de escribir archivos de texto en C++. Por ejemplo, se puede utilizar la función `std::endl` para separar líneas en el archivo, o también se pueden escribir varios tipos de datos utilizando la sobrecarga del operador `<<`.

También es importante tener en cuenta el manejo de errores al escribir archivos. Es necesario comprobar si el archivo se ha abierto correctamente y si se han escrito todos los datos de manera exitosa.

## Ver también
- [Página de la librería std::fstream en cplusplus.com](http://www.cplusplus.com/reference/fstream/)
- [Tutorial de escritura de archivos de texto en C++](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [Ejemplos de escritura de archivos de texto en C++](https://www.programiz.com/cpp-programming/file-operation)