---
title:                "C++: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

#¿Por qué leer un archivo de texto en C++?

Leer un archivo de texto es una tarea común en muchos programas de C++. Puede ser útil para leer datos de entrada o para escribir resultados en un archivo. En esta publicación, exploraremos cómo leer un archivo de texto en C++ y por qué es importante saber cómo hacerlo.

## Cómo hacerlo

Primero, necesitamos incluir la biblioteca `fstream` en nuestro programa para poder trabajar con archivos. Luego, creamos un objeto `ifstream` y abrimos el archivo que queremos leer. A continuación, podemos utilizar la función `getline()` para leer cada línea del archivo y guardarla en una variable. Finalmente, cerramos el archivo una vez que hemos terminado de leer.

```C++
#include <fstream>

// Creamos un objeto ifstream y abrimos el archivo
ifstream archivo("datos.txt");

string linea; // Variable para guardar cada línea leída del archivo

while (getline(archivo, linea)) // Leemos una línea del archivo y la guardamos en "linea"
{
    cout << linea << endl; // Imprimimos la línea leída en la consola
}

archivo.close(); // Cerramos el archivo
```

Suponiendo que nuestro archivo `datos.txt` contiene las siguientes líneas:

```
Juan
Maria
Pedro
```

El resultado de este código sería:

```
Juan
Maria
Pedro
```

## Profundizando

Existen otras formas de leer archivos de texto en C++, como utilizar la función `get()` para leer carácter por carácter en lugar de línea por línea. También podemos utilizar la función `getlines()` para guardar todas las líneas en un vector en lugar de imprimir directamente en la consola. Además, es importante manejar posibles errores al abrir o cerrar el archivo utilizando bloques `try-catch`.

También podemos realizar operaciones en los datos leídos, como convertirlos a otros tipos de datos o realizar cálculos. Además, podemos escribir en un archivo de salida utilizando un objeto `ofstream` y la función `push_back()` para agregar nuevos datos al archivo.

¡Hay muchas posibilidades cuando se trata de leer y trabajar con archivos de texto en C++!

# Ver también

- [Documentación de C++ para la biblioteca `fstream`](https://en.cppreference.com/w/cpp/header/fstream)
- [Tutorial en video sobre cómo leer archivos de texto en C++](https://www.youtube.com/watch?v=j0Lhd4muxTY)
- [Ejemplos de código para leer y escribir archivos en C++](https://www.programiz.com/cpp-programming/library-function/fstream)