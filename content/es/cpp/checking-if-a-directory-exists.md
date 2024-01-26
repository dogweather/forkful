---
title:                "Comprobando si existe un directorio"
html_title:           "Bash: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Verificar si un directorio existe implica confirmar su presencia en el sistema de archivos antes de realizar operaciones con él. Los programadores hacen esto para evitar errores al intentar acceder a directorios que tal vez no estén disponibles, lo que es esencial para la estabilidad y fiabilidad del programa.

## Cómo hacerlo:

Con C++17, `std::filesystem` entra al juego, simplificando las cosas. Aquí te dejo un ejemplo compacto:

```C++
#include <iostream>
#include <filesystem>

int main() {
    std::filesystem::path dirPath = "/path/to/directory";

    if(std::filesystem::exists(dirPath)) {
        std::cout << "El directorio existe!" << std::endl;
    } else {
        std::cout << "El directorio no existe!" << std::endl;
    }

    return 0;
}
```

Si el directorio existe, verás:

```
El directorio existe!
```

En caso contrario:

```
El directorio no existe!
```

## Inmersión Profunda:

Antes de C++17, esta tarea era más propensa a errores, dependiendo de funciones del sistema operativo o de bibliotecas de terceros. `boost::filesystem` era una opción popular.

Hay alternativas, como `stat` en POSIX y `GetFileAttributes` en Windows. Sin embargo, `std::filesystem` vino a unificar y simplificar este proceso.

El chequeo de existencia en sí no es costoso, pero si se hace repetidamente en un bucle o en un entorno de múltiples hilos, podría ser mejor pensar en un enfoque de manejo de errores más sofisticado para no detener la ejecución innecesariamente.

## Ver También:

Para profundizar, consulta los siguientes enlaces:

- Documentación `std::filesystem` de C++17: https://en.cppreference.com/w/cpp/filesystem
- Una mirada a `boost::filesystem` (predecesor de `std::filesystem`): https://www.boost.org/doc/libs/release/libs/filesystem/
- `stat` de POSIX: http://pubs.opengroup.org/onlinepubs/009695399/functions/stat.html
- `GetFileAttributes` de Windows API: https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfileattributes
