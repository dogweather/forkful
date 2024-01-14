---
title:    "C++: Creando un archivo temporal"
keywords: ["C++"]
---

{{< edit_this_page >}}

### ¿Por qué crear un archivo temporal en C++?

La creación de archivos temporales es una práctica común en la programación en C++. Los archivos temporales se utilizan para almacenar datos temporales o intermedios durante la ejecución del programa. Esto ayuda a mantener los archivos principales del programa ordenados y a evitar la sobrecarga de memoria.

### Cómo crear un archivo temporal en C++

Para crear un archivo temporal en C++, se pueden seguir estos pasos:

1. Incluir la biblioteca `fstream` en el programa.
2. Crear un objeto `ofstream` y nombrarlo como se desee (por ejemplo, `tempfile`).
3. Utilizar la función `tmpnam()` para generar un nombre único para el archivo temporal.
4. Abrir el archivo temporal utilizando la función `open()` y pasarle el nombre generado por `tmpnam()`.
5. Escribir los datos en el archivo utilizando la función `<<`.
6. Cerrar el archivo utilizando la función `close()`.

```C++
#include <fstream>
using namespace std;

// Crear objeto ofstream
ofstream tempfile;
// Generar nombre único para el archivo temporal
char filename[L_tmpnam];
tmpnam(filename);
// Abrir el archivo temporal
tempfile.open(filename);
// Escribir datos en el archivo
tempfile << "Este es un archivo temporal creado para demostrar su uso en C++.";
// Cerrar el archivo
tempfile.close();
```

La salida de este código sería un archivo temporal con el contenido escrito.

### Profundizando en la creación de archivos temporales

La función `tmpnam()` genera un nombre único para el archivo temporal, pero su uso no está exento de riesgos. Debido a que el nombre generado es solo una cadena de caracteres, puede ser vulnerable a ataques maliciosos si se utiliza en aplicaciones que manejan datos sensibles.

Una solución a esto es utilizar la función `tmpfile()` en su lugar, que genera un archivo temporal seguro y único, y devuelve un puntero a un objeto `FILE`. Sin embargo, esta función solo admite la escritura de datos en el archivo y no la lectura de los mismos.

Otra opción es utilizar la biblioteca `filesystem` de C++17, que proporciona una función `temp_directory_path()` que devuelve una ruta de directorio seguro para almacenar archivos temporales.

### Ver también

- [Creación de archivos en C++](https://www.programiz.com/cpp-programming/files-io)
- [Biblioteca fstream en C++](https://www.cplusplus.com/reference/fstream/)
- [Función tmpnam en C++](https://www.cplusplus.com/reference/cstdio/tmpnam/)
- [Biblioteca filesystem en C++17](https://en.cppreference.com/w/cpp/filesystem)