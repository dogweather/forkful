---
title:                "Escribiendo un archivo de texto"
html_title:           "C++: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Escribir un archivo de texto es simplemente guardar texto en un formato legible para las computadoras. Los programadores lo hacen para almacenar información que se pueda leer y procesar fácilmente por otros programas o personas.

## Cómo:
Utilizando el lenguaje de programación C++, podemos escribir un archivo de texto utilizando la biblioteca estándar `ofstream`. Primero, debemos incluir la biblioteca en nuestro programa y luego crear un objeto de archivo utilizando el nombre que queramos para nuestro archivo.
```C++
#include <fstream>
ofstream miArchivo;

```
Luego, podemos escribir texto en nuestro archivo utilizando el operador `<<` y el método `close()` para guardar y cerrar el archivo.
```C++
miArchivo << "¡Hola mundo!" << endl;
miArchivo.close();

```
Si deseamos agregar texto al final de un archivo existente en lugar de sobrescribirlo, podemos usar el modo "append" al crear el objeto de archivo.
```C++
ofstream miArchivo("miArchivo.txt", ios::app);
```

## Deep Dive:
Escribir archivos de texto ha sido una función crucial en la programación desde los primeros días de las computadoras. Antes de los sistemas operativos modernos, los programas se escribían como archivos de texto e ingresaban al computador a través de tarjetas perforadas. Aunque ahora existen otras opciones como bases de datos y archivos binarios, los archivos de texto siguen siendo una forma fácil y universal de almacenar información.

## Consulte también:
- [Documentación de la biblioteca estándar de C++](https://www.cplusplus.com/reference/fstream/ofstream/)
- [Tutorial de C++ sobre archivos de texto](https://www.learncpp.com/cpp-tutorial/186-basic-file-io/)
- [Historia de los archivos de texto](https://www.britannica.com/technology/text-file)