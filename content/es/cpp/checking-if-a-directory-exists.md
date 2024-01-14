---
title:                "C++: Comprobando si existe un directorio"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué 

Hay ocasiones en las que necesitamos asegurarnos de que una carpeta o directorio existe antes de realizar alguna acción en nuestro código. Esto puede ahorrarnos tiempo y evitar errores en nuestro programa.

## Cómo

Para comprobar si existe una carpeta en C++, podemos utilizar la clase `std::filesystem::path` de la biblioteca de archivos estándar (filesystem). Esta clase nos permite trabajar con rutas y archivos en nuestro sistema de archivos.

Primero, debemos incluir la biblioteca en nuestro código:

```C++
#include <filesystem>
```

Una vez que hemos incluido la biblioteca, podemos crear un objeto de la clase `std::filesystem::path` con la ruta de la carpeta que queremos verificar.

```C++
std::filesystem::path myFolder("ruta/a/mi/carpeta");
```

Luego, podemos comprobar si la carpeta existe utilizando la función `exists()` de la clase `std::filesystem::path`.

```C++
if (myFolder.exists()) {
  // La carpeta existe
} else {
  // La carpeta no existe
}
```

## Deep Dive

La función `exists()` devuelve un valor booleano `true` si la carpeta existe y `false` si no existe. También podemos utilizar la función `is_directory()` para asegurarnos de que la ruta especificada es una carpeta y no un archivo.

Además, la clase `std::filesystem::path` ofrece otras funciones útiles para trabajar con rutas y archivos, como `create_directory()` para crear una carpeta y `remove()` para eliminar un archivo o carpeta.

## Ver también

- [Documentación de std::filesystem en cppreference](https://en.cppreference.com/w/cpp/filesystem)
- [Tutorial de Programiz sobre std::filesystem](https://www.programiz.com/cpp-programming/filesystem)