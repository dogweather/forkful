---
title:                "Comprobando si existe un directorio."
html_title:           "C++: Comprobando si existe un directorio."
simple_title:         "Comprobando si existe un directorio."
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

Hay varias situaciones en las que puede ser útil verificar si un directorio existe en un programa de C++. Por ejemplo, al crear una aplicación que lee y guarda archivos, es importante asegurarse de que el directorio de destino exista antes de intentar guardar los datos. También puede ser importante asegurarse de que el usuario haya proporcionado un directorio válido al especificar la ubicación de un archivo o algoritmo.

## Cómo hacerlo

Para verificar si un directorio existe en C++, puedes utilizar la función ```std::filesystem::exists()``` de la biblioteca estándar. Esta función toma como argumento una cadena de caracteres que representa la ruta del directorio y devuelve un valor booleano que indica si el directorio existe o no.

A continuación se muestra un ejemplo de código que verifica si un directorio llamado "Archivos" existe en el directorio actual y muestra un mensaje en consecuencia:

```C++
#include <iostream>
#include <filesystem>

int main() {
    std::string directorio = "Archivos";
    if (std::filesystem::exists(directorio)) {
        std::cout << "El directorio \"" << directorio << "\" existe.";
    } else {
        std::cout << "El directorio \"" << directorio << "\" no existe.";
    }
    return 0;
}
```

Output:

```
El directorio "Archivos" no existe.
```

Ten en cuenta que también puedes utilizar la función ```std::filesystem::is_directory()``` para verificar si un determinado archivo es un directorio. Esta función también devuelve un valor booleano, pero en este caso indica si el archivo es un directorio o no.

## Una mirada más profunda

La función ```std::filesystem::exists()``` utiliza la biblioteca ```<filesystem>``` recientemente agregada en la versión C++17 del lenguaje. Antes de esta versión, había varias formas de verificar si un directorio existe en C++, pero no era una solución estándar y portátil.

La biblioteca ```<filesystem>``` proporciona una interfaz más intuitiva y fácil de usar para manipular archivos y directorios en C++. Además de funciones para verificar la existencia de un directorio, también ofrece funciones para enumerar los contenidos de un directorio, crear o eliminar directorios, y mucho más. Si estás interesado en aprender más sobre esta biblioteca y sus funciones, puedes seguir estos enlaces:

- [Referencia de la biblioteca <filesystem> de C++ en cppreference.com](https://en.cppreference.com/w/cpp/filesystem)
- [Tutorial de la biblioteca de archivos y directorios en C++ en cplusplus.com](http://www.cplusplus.com/doc/tutorial/files/)

## Ver también

- [La biblioteca <filesystem> en la documentación oficial de C++](https://docs.microsoft.com/en-us/cpp/standard-library/filesystem?view=msvc-160)
- [Pregunta relacionada en el sitio Stack Overflow sobre cómo verificar si un directorio existe en C++](https://stackoverflow.com/questions/26411130/how-can-i-check-if-a-file-is-a-directory-in-c)