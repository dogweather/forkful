---
title:                "Comprobando si un directorio existe"
html_title:           "C++: Comprobando si un directorio existe"
simple_title:         "Comprobando si un directorio existe"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Verificar si un directorio existe en C++ implica confirmar si existe una ruta de directorio específica en nuestro sistema de archivos. Esto es vital para evitar errores al intentar acceder a un directorio que no existe.

## Cómo hacerlo:

Un ejemplo de cómo verificar la existencia de un directorio utilizando la librería `<filesystem>` de C++, introducida en C++17, es el siguiente:

```C++
#include <filesystem>
#include <iostream>

int main() {
     std::filesystem::path p{"ruta/de/directorio"};

     if (std::filesystem::exists(p)) {
        std::cout << "El directorio existe.\n";
     } 
     else {
        std::cout << "El directorio no existe.\n";
     }

    return 0;
}
```
Si el directorio existe, el programa imprimirá 'El directorio existe' en la consola. Si el directorio no existe, imprimirá 'El directorio no existe'.

## Inmersión Profunda:

Históricamente, verificar la existencia de un directorio en C++ ha sido laborioso, demandando la inclusión de librerías dependientes de la plataforma y código extra. Con la introducción de `<filesystem>` en C++17, se ha facilitado enormemente.

Existen alternativas como la función `stat` y `opendir` en POSIX o `GetFileAttributes` en Windows, aunque requieren más código y complejidad.

La función `std::filesystem::exists` retorna un booleano indicando si la ruta de archivo o directorio proporcionada existe realmente. Es importante tener en cuenta que la función puede fallar si no se tienen suficientes permisos para acceder a la ruta.

## Ver También:

Para una comprensión más profunda de la librería de sistema de archivos en C++, puedes ver la documentación oficial en cppreference: https://en.cppreference.com/w/cpp/filesystem

Para los temas relacionados con las funciones de directorio en POSIX, el siguiente enlace será útil: https://pubs.opengroup.org/onlinepubs/9699919799/functions/directory.html