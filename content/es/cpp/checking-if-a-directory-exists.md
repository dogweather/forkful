---
title:    "C++: Comprobando si existe un directorio"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, a menudo es necesario verificar si una carpeta o directorio existe antes de realizar cualquier operación en él. Esto puede optimizar el rendimiento y evitar posibles errores en el código.

## Cómo hacerlo

Para verificar si un directorio existe en C++, podemos utilizar la función `opendir ()` del header `dirent.h`. Esta función retorna un puntero al directorio si existe o un valor nulo si no existe. Aquí hay un ejemplo de cómo podemos implementarlo:

```C++
#include <dirent.h>
#include <iostream>
using namespace std;
 
int main() {
   DIR *dir;
   string path = "/ruta/al/directorio";

   dir = opendir(path.c_str());

   if (dir != NULL) {
   	   cout << "¡El directorio existe!" << endl;
   	   closedir(dir);
   } else {
   	   cout << "¡El directorio no existe!" << endl;
   }   
   return 0;
}
```

En este ejemplo, primero incluimos el header `dirent.h` que nos da acceso a la función `opendir()`. Luego, definimos la ruta del directorio que queremos comprobar en la variable `path`. Después, llamamos a la función `opendir()` pasando la ruta del directorio como argumento. Si la función retorna un puntero distinto de nulo, eso significa que el directorio existe. Cerramos el puntero con la función `closedir()` para liberar memoria y evitar posibles errores en el futuro.

Si el directorio no existe, la función `opendir()` retornará un valor nulo, por lo que podemos utilizar una sentencia `if` para imprimir un mensaje de acuerdo al resultado.

## Profundizando en la verificación de directorios

Además de la función `opendir()`, también podemos utilizar otras técnicas para verificar si un directorio existe. Por ejemplo, podemos utilizar el comando de sistema `mkdir()` para crear un directorio en la ruta especificada y luego eliminarlo. Si el directorio ya existe, el comando retornará un error y podemos utilizar esa información para determinar si el directorio existe.

También podemos utilizar la librería `boost::filesystem` para verificar la existencia de un directorio en C++. Esta librería nos ofrece funciones más avanzadas para manejar la manipulación de archivos y directorios.

## Ver también

- [Cómo verificar si un archivo existe en C++](https://www.freecodecamp.org/news/how-to-check-if-a-file-exists-in-c/)

- [Documentación de la función `opendir()`](https://www.man7.org/linux/man-pages/man3/opendir.3.html) 

- [Guía de referencia de la librería `boost::filesystem`](https://www.boost.org/doc/libs/1_76_0/libs/filesystem/doc/reference.html#DirectoryIteration)