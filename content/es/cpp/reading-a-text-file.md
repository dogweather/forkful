---
title:                "C++: Leyendo un archivo de texto"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, a menudo necesitamos leer información almacenada en archivos de texto para procesarla en nuestros programas. Por lo tanto, es importante comprender cómo podemos leer archivos de texto utilizando C++, especialmente si estamos trabajando en proyectos que involucran la manipulación de grandes cantidades de datos.

## Cómo hacerlo

Para leer un archivo de texto en C++, primero debemos incluir la biblioteca "fstream". Luego, podemos usar objetos de la clase "ifstream" para abrir y leer el archivo. Aquí hay un ejemplo de código que lee un archivo de texto y muestra su contenido en la pantalla:

```C++
#include <iostream>
#include <fstream>

int main() {
    // Abrimos el archivo de texto
    std::ifstream archivo("ejemplo.txt");

    // Verificamos si el archivo se abrió correctamente
    if (!archivo) {
        std::cout << "No se pudo abrir el archivo" << std::endl;
        return 1;
    }

    // Leemos el archivo y mostramos su contenido en la pantalla
    std::string linea;
    while (std::getline(archivo, linea)) {
        std::cout << linea << std::endl;
    }

    // Cerramos el archivo
    archivo.close();

    return 0;
}
```

Este ejemplo utiliza un bucle "while" y la función "getline" para leer línea por línea del archivo de texto y mostrar cada línea en la pantalla. Como resultado, podríamos ver algo así:

```
Este es un ejemplo de archivo de texto.
Cada línea se mostrará en la pantalla.
Podemos leer y procesar esta información en nuestros programas.
```

## Una mirada más profunda

Además de simplemente leer y mostrar el contenido de un archivo de texto, también podemos realizar otras operaciones, como escribir en el archivo y buscar información específica en él. Para ello, podemos utilizar diferentes funciones y métodos de la clase "ifstream" según nuestras necesidades.

Por ejemplo, si queremos buscar una palabra o frase específica en el archivo de texto, podemos utilizar la función "find" de la clase "string" para encontrarla en cada línea leída y realizar alguna acción en consecuencia. También podemos escribir en el archivo utilizando la clase "ofstream" y la función "write" para agregar contenido nuevo o sobrescribir el contenido existente.

## Ver también

- La documentación oficial de C++ para la clase "ifstream": https://www.cplusplus.com/reference/fstream/ifstream/
- Otros ejemplos y tutoriales sobre cómo leer y escribir archivos de texto en C++: https://www.programiz.com/cpp-programming/files-input-output