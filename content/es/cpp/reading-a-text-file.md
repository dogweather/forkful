---
title:    "C++: Leyendo un archivo de texto"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

# ¿Por qué leer un archivo de texto?

Hay muchas razones por las que podrías querer leer un archivo de texto en tu programa de C++. Quizás estás buscando datos específicos para analizar, o tal vez necesitas utilizar los datos para realizar alguna operación matemática. Sea cual sea la razón, saber cómo leer un archivo de texto es una habilidad útil para cualquier programador de C++.

## Cómo hacerlo

Para leer un archivo de texto en C++, necesitarás utilizar la biblioteca estándar "fstream". Esta biblioteca contiene las clases ifstream (input file stream) y ofstream (output file stream) que te permitirán abrir y leer un archivo de texto.

Primero, deberás incluir la biblioteca en tu programa utilizando la línea de código `#include <fstream>`.

Luego, puedes utilizar la clase ifstream para abrir el archivo de texto que desees leer. Por ejemplo, si tienes un archivo llamado "datos.txt" en la misma carpeta que tu programa, puedes abrirlo con el siguiente código:

```C++
ifstream archivo("datos.txt");
```

Una vez que hayas abierto el archivo, puedes utilizar la función `getline()` para leer una línea completa del archivo en una variable string. Puedes repetir esta función hasta que hayas leído todo el contenido del archivo.

Finalmente, no olvides cerrar el archivo utilizando `archivo.close()` cuando hayas terminado de leerlo.

## Profundizando

Leer un archivo de texto puede volverse un poco más complicado si el archivo contiene diferentes tipos de datos (como enteros y strings). En este caso, es importante que sepas cómo separarlos y almacenarlos correctamente.

Una forma de hacerlo es utilizando la función `>>` de la clase ifstream. Por ejemplo, si tienes un archivo con una lista de nombres y edades separados por un espacio en cada línea, puedes almacenarlos en variables usando el siguiente código:

```C++
string nombre;
int edad;

archivo >> nombre >> edad;
```

Esto asignará la primera palabra de cada línea a la variable "nombre" y la segunda palabra a la variable "edad".

Además, es importante manejar los errores que puedan surgir al leer un archivo de texto, como por ejemplo si el archivo no existe o si hay un error al abrirlo. Puedes hacer esto utilizando las funciones `good()` y `bad()` de la clase ifstream para verificar si la lectura del archivo fue exitosa o no.

En resumen, leer un archivo de texto puede implicar algunas consideraciones adicionales, pero es una habilidad útil que te permitirá trabajar con una gran cantidad de datos en tus programas de C++.

## Ver también

- [Tutorial de C++ para principiantes](https://www.programiz.com/cpp-programming)
- [Documentación de la clase ifstream](https://www.cplusplus.com/reference/fstream/ifstream/)
- [Ejemplos de lectura y escritura de archivos en C++](https://www.geeksforgeeks.org/reading-writing-text-files-c/)