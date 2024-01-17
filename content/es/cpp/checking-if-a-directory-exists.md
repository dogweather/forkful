---
title:                "Comprobando si existe un directorio"
html_title:           "C++: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Qué y por qué?
La comprobación de si un directorio existe es una parte esencial de la programación en C++. Es una técnica que se utiliza para verificar si un directorio o carpeta específica existe en un sistema de archivos antes de realizar cualquier operación en ella. Los programadores realizan esta comprobación para evitar errores y manejar de manera adecuada casos en los que el directorio no exista.

## Cómo:
```C++
#include <iostream>
#include <filesystem>

int main() {
    // Ejemplo de una comprobación de existencia de directorio
    std::filesystem::path directorio("directorio_ejemplo");

    if (std::filesystem::exists(directorio)) {
        std::cout << "El directorio existe" << std::endl;
    } else {
        std::cout << "El directorio no existe" << std::endl;
    }

    return 0;
}
```
**Salida:**
```
El directorio existe
```

## Profundizando:
La comprobación de si un directorio existe es una técnica que ha sido utilizada por programadores desde los primeros años de la programación. Inicialmente, se utilizaba para verificar la existencia de un directorio antes de descargar o cargar archivos en él. Con el avance de la tecnología, esta técnica también se ha vuelto importante para asegurar la integridad de los sistemas de archivos.

Otra alternativa para comprobar la existencia de un directorio es mediante el uso de funciones como `access()` o `stat()`, que también son populares entre los programadores. Sin embargo, la función `exists()` de la biblioteca `<filesystem>` de C++ ofrece una forma más elegante y legible de realizar esta comprobación.

En cuanto a la implementación, la función `exists()` devuelve un valor booleano verdadero si el directorio existe y falso si no es así. También se pueden pasar opciones adicionales para verificar si el directorio es accesible, si es un archivo regular o si es un enlace simbólico.

## Vea también:
- Documentación de la biblioteca `<filesystem>` de C++: https://devdocs.io/cpp/filesystem
- Cómo comprobar la existencia de un archivo en C++: https://www.programiz.com/cpp-programming/files-input-output
- Información sobre la función `exists()` de la biblioteca `<filesystem>`: https://www.learncpp.com/cpp-tutorial/186-basic-file-io/