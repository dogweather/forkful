---
title:                "C++: Escribir un archivo de texto"
simple_title:         "Escribir un archivo de texto"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir un archivo de texto en C++?

Escribir un archivo de texto en C++ es una habilidad esencial para cualquier programador. Esto nos permite almacenar y acceder a datos importantes de forma organizada y legible. Además, puede ser utilizado en una variedad de aplicaciones, desde guardar registros de datos hasta crear archivos de configuración para programas.

## Cómo escribir un archivo de texto en C++

Para escribir un archivo de texto en C++, primero necesitamos incluir la biblioteca de manejo de archivos `<fstream>`. A continuación, abrimos el archivo en el que queremos escribir utilizando el objeto `ofstream`, especificando el nombre del archivo y el modo de escritura. Luego, podemos escribir en el archivo utilizando el operador de inserción `<<` y cerrar el archivo cuando hayamos terminado. A continuación, se muestra un ejemplo de código para escribir una línea en un archivo de texto:

```C++
#include <fstream>

using namespace std;

int main() {
    ofstream archivo("ejemplo.txt"); // Se crea el archivo en modo escritura
    archivo << "¡Hola mundo!"; // Escribimos en el archivo
    archivo.close(); // Cerramos el archivo
    return 0;
}
```

Si ejecutamos este código, aparecerá un archivo llamado "ejemplo.txt" con el contenido "¡Hola mundo!".

## Profundización en la escritura de archivos de texto

Existen varias funciones y métodos que nos permiten tener un mayor control sobre la escritura de archivos de texto en C++. Por ejemplo, podemos utilizar el método `write` para escribir una cadena específica de caracteres en el archivo, o el método `put` para escribir un único carácter.

Además, podemos utilizar distintos modos de apertura al crear el objeto `ofstream`. Por ejemplo, si utilizamos `ofstream archivo("ejemplo.txt", ios::app)`, el archivo se abrirá en modo de "agregar" en lugar de en modo de "escritura", lo que nos permitirá añadir contenido al final del archivo en lugar de sobrescribirlo.

También podemos utilizar un bucle para escribir múltiples líneas o contenidos en un archivo de texto. Y, por supuesto, también podemos leer un archivo de texto utilizando la biblioteca `<fstream>` y el objeto `ifstream`.

## Ver también

- [Guía de referencia de C++ para abrir y escribir en archivos de texto](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [Documentación de la biblioteca <fstream> en C++](https://www.cplusplus.com/reference/fstream/)
- [Ejemplos de escritura y lectura de archivos de texto en C++](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)