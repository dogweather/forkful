---
title:                "Escritura de un archivo de texto"
date:                  2024-01-19
html_title:           "Bash: Escritura de un archivo de texto"
simple_title:         "Escritura de un archivo de texto"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir en un archivo de texto es guardar datos en un archivo legible por humanos. Programadores lo hacen para registrar información, como logs, configurar programas o exportar datos.

## Cómo hacerlo:
Ejemplo de código para escribir en un archivo de texto en C++:

```C++
#include <fstream>
#include <iostream>

int main() {
    // Crear y abrir un archivo de texto
    std::ofstream archivo("ejemplo.txt");

    // Verificar si el archivo fue abierto correctamente
    if (archivo.is_open()) {
        // Escribir texto en el archivo
        archivo << "Hola, Mundo!\n";
        archivo << "Esto es una prueba de escritura de archivo.";
        
        // Cerrar el archivo
        archivo.close();
    } else {
        std::cerr << "No se pudo abrir el archivo." << std::endl;
    }

    return 0;
}
```

Salida en `ejemplo.txt`:
```
Hola, Mundo!
Esto es una prueba de escritura de archivo.
```

## Análisis Detallado:
Historicamente, archivos de texto se usan como una manera sencilla y universal de almacenar y compartir información. C++ ofrece varias alternativas para escribir en archivos, como bibliotecas de alto nivel (como Boost.IOStreams) o la API de C (usando `fprintf`, por ejemplo). La clase `std::ofstream` es parte de la biblioteca estándar (STL) y encapsula los detalles de implementación, dando al programador una interfaz simple para trabajar con archivos.

## Ver También:
- Documentación de C++ `std::ofstream`: https://en.cppreference.com/w/cpp/io/basic_ofstream
- Guía sobre I/O de archivos en C++: https://www.cplusplus.com/doc/tutorial/files/
- Tutorial de Boost.IOStreams: https://www.boost.org/doc/libs/release/libs/iostreams/doc/index.html
- Preguntas sobre archivos en StackOverflow: https://stackoverflow.com/questions/tagged/file-io+c%2b%2b
