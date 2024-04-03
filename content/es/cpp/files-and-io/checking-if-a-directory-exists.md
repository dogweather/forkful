---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:44.774452-07:00
description: "Comprobar si un directorio existe trata de determinar la presencia de\
  \ un directorio en una ruta especificada antes de realizar operaciones como leer\
  \ o\u2026"
lastmod: '2024-03-13T22:44:59.389259-06:00'
model: gpt-4-0125-preview
summary: "Comprobar si un directorio existe trata de determinar la presencia de un\
  \ directorio en una ruta especificada antes de realizar operaciones como leer o\
  \ escribir en archivos dentro de \xE9l."
title: Comprobando si un directorio existe
weight: 20
---

## Cómo hacerlo:
En C++ moderno (C++17 y posteriores), puedes usar la biblioteca de sistema de archivos para comprobar si un directorio existe. Ofrece una manera directa y estandarizada de realizar operaciones del sistema de archivos, incluyendo la verificación de la existencia de un directorio.

```cpp
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    const fs::path dirPath = "/ruta/al/directorio";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "El directorio existe." << std::endl;
    } else {
        std::cout << "El directorio no existe." << std::endl;
    }

    return 0;
}
```
Salida de muestra si el directorio existe:
```
El directorio existe.
```

Salida de muestra si el directorio no existe:
```
El directorio no existe.
```

Para proyectos que aún no utilizan C++17 o para características adicionales, la biblioteca Boost Filesystem es una opción de terceros muy popular que ofrece una funcionalidad similar.

```cpp
#include <iostream>
#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

int main() {
    const fs::path dirPath = "/ruta/al/directorio";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "El directorio existe." << std::endl;
    } else {
        std::cout << "El directorio no existe." << std::endl;
    }

    return 0;
}
```
Usando Boost Filesystem, la salida sería idéntica al ejemplo del sistema de archivos de C++17, dependiendo de la existencia del directorio en la ruta especificada.
