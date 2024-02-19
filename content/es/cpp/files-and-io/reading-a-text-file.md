---
aliases:
- /es/cpp/reading-a-text-file/
date: 2024-01-20 17:54:04.089128-07:00
description: "Leer un archivo de texto en C++ significa extracci\xF3n de datos desde\
  \ un archivo en tu disco hacia tu programa. Programadores lo hacen para manejar\u2026"
lastmod: 2024-02-18 23:09:10.330737
model: gpt-4-1106-preview
summary: "Leer un archivo de texto en C++ significa extracci\xF3n de datos desde un\
  \ archivo en tu disco hacia tu programa. Programadores lo hacen para manejar\u2026"
title: Lectura de un archivo de texto
---

{{< edit_this_page >}}

## Qué & Por Qué?
Leer un archivo de texto en C++ significa extracción de datos desde un archivo en tu disco hacia tu programa. Programadores lo hacen para manejar información persistente o compartir datos entre procesos o incluso entre diferentes ejecuciones de programas.

## Cómo hacerlo:
Aquí tienes un ejemplo sencillo de cómo leer un archivo de texto línea por línea utilizando `fstream`:

```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream archivo("ejemplo.txt");
    std::string linea;

    if (archivo.is_open()) {
        while (getline(archivo, linea)) {
            std::cout << linea << '\n';
        }
        archivo.close();
    } else {
        std::cout << "No se pudo abrir el archivo." << std::endl;
    }
    
    return 0;
}
```

Salida de muestra si `ejemplo.txt` contiene:
```
Hola, este es un archivo de ejemplo.
Segunda línea del archivo.
```

Sería:
```
Hola, este es un archivo de ejemplo.
Segunda línea del archivo.
```

## Profundización:
En el pasado, leer archivos en C++ era más engorroso y menos seguro debido a la necesidad de manejar el archivo como un array de caracteres puro. Con la estandarización de la biblioteca de C++, `fstream` se convirtió en la herramienta estándar para la E/S de archivos debido a su encapsulamiento y manejo de recursos.

Alternativas modernas a `fstream` incluyen bibliotecas como Boost.Iostreams o el uso de operaciones de sistema específicas del OS. Respecto a la implementación, `fstream` se basa en las funciones de más bajo nivel proporcionadas por C, ofreciendo una interfaz segura de tipo orientado a objetos sobrepuesta a estas.

Una cuestión clave a considerar es la gestión de errores; verificar `is_open()` es importante para evitar problemas al abrir archivos. Otra práctica importante es siempre cerrar el archivo con `close()` para liberar recursos, aunque los objetos de flujo de archivo lo harán automáticamente al salir de su ámbito.

## Ver También:
- Documentación oficial de cppreference sobre fstream: [https://en.cppreference.com/w/cpp/io/basic_fstream](https://en.cppreference.com/w/cpp/io/basic_fstream)
- Un tutorial más detallado sobre la lectura de archivos: [https://www.cplusplus.com/doc/tutorial/files/](https://www.cplusplus.com/doc/tutorial/files/)
- Información sobre Boost.Iostreams: [https://www.boost.org/doc/libs/release/libs/iostreams/](https://www.boost.org/doc/libs/release/libs/iostreams/)
