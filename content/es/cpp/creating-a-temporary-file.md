---
title:                "C++: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##Por qué

Crear un archivo temporal puede ser una tarea común en la programación. Puede ser utilizado para almacenar datos temporales, hacer pruebas o incluso como parte de una solución más compleja. ¡Aprende cómo crear archivos temporales con C++!

##Cómo hacerlo

La creación de un archivo temporal en C++ es posible utilizando la biblioteca estándar `<fstream>`. Primero, debes incluir esta biblioteca en tu código. Luego, puedes usar la función `tmpnam()` para generar un nombre único para tu archivo temporal. Aquí hay un ejemplo de cómo crear y escribir en un archivo temporal:

```C++
#include <iostream>
#include <fstream>

int main() {
    char filename[L_tmpnam]; // crea un array con el tamaño del nombre del archivo temporal
    tmpnam(filename); // genera el nombre del archivo temporal

    std::ofstream archivo; // crea un objeto ofstream
    archivo.open(filename); // abre el archivo temporal
    archivo << "Este es un archivo temporal generado en C++"; // escribe en el archivo
    archivo.close(); // cierra el archivo

    return 0;
}
```

El resultado de este código será un archivo llamado "tmp.XXXXXX" (las "X" representan números y/o letras aleatorias). Si lo abres, verás el texto que hemos escrito en él.

##Profundizando

Si quieres tener más control sobre tu archivo temporal, puedes utilizar la función `tmpfile()` en lugar de `tmpnam()`. Esta función creará un archivo y te proporcionará un puntero al mismo, lo que te permitirá leer y escribir en él. Además, puedes utilizar la función `remove()` para eliminar el archivo temporal cuando ya no sea necesario.

¡Ahora estás listo para empezar a utilizar archivos temporales en tus proyectos de C++!

##Ver también

- [Documentación sobre la biblioteca estándar de C++](https://www.cplusplus.com/)
- [Más información sobre la función `tmpnam()`](https://www.cplusplus.com/reference/cstdio/tmpnam/)
- [Más información sobre la función `tmpfile()`](https://www.cplusplus.com/reference/cstdio/tmpfile/)
- [Más información sobre la función `remove()`](https://www.cplusplus.com/reference/cstdio/remove/)