---
aliases:
- /es/cpp/checking-if-a-directory-exists/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:44.774452-07:00
description: "Comprobar si un directorio existe trata de determinar la presencia de\
  \ un directorio en una ruta especificada antes de realizar operaciones como leer\
  \ o\u2026"
lastmod: 2024-02-18 23:09:10.327347
model: gpt-4-0125-preview
summary: "Comprobar si un directorio existe trata de determinar la presencia de un\
  \ directorio en una ruta especificada antes de realizar operaciones como leer o\u2026"
title: Comprobando si un directorio existe
---

{{< edit_this_page >}}

## Qué y Por Qué?
Comprobar si un directorio existe trata de determinar la presencia de un directorio en una ruta especificada antes de realizar operaciones como leer o escribir en archivos dentro de él. Los programadores lo hacen para evitar errores relacionados con las operaciones de archivo, asegurando una ejecución más suave y fiable de las tareas de manejo de archivos en sus aplicaciones.

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
