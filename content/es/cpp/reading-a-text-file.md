---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Leer un archivo de texto en la programación es el proceso de extracción de datos desde un archivo en el sistema de almacenamiento. Lo hacemos para recuperar y manipular los datos almacenados de forma persistente para diversas tareas.

## ¿Cómo?

Para leer un archivo de texto en C++, necesitamos implicar la biblioteca fstream. Aquí tienes un ejemplo generalizado de cómo funciona.

```C++
#include <iostream>
#include <fstream> 
using namespace std; 

int main () {
    ifstream archivo ("ejemplo.txt"); 
    string linea;
    if(archivo.is_open()) {
        while (getline(archivo, linea)) { 
            cout << linea << '\n'; 
        }
        archivo.close(); 
    } else cout << "No se puede abrir el archivo"; 

    return 0; 
}
```

Este sencillo código abre el archivo de texto denominado 'ejemplo.txt' y lo lee línea por línea. Cada línea se imprime en la consola hasta que se alcanza el final del archivo.

## Profundización

1. **Historia**: Los primeros lenguajes de programación, como el Fortran, ya tenían mecanismos para la lectura y escritura de archivos de texto en la década de 1960.

2. **Alternativas**: En C++, la lectura de archivos se puede realizar usando fstream, ifstream y otros. La elección de cuál depende de los requerimientos específicos del programa. Otra forma popular es utilizando la biblioteca Boost con su módulo filesystem.

3. **Detalles de Implementación**: C++ utiliza los flujos de entrada/salida para trabajar con archivos. La palabra 'ifstream' viene de 'input file stream' y 'ofstream' de 'output file stream'. Cuando se llama a la función 'getline', esta lee la línea completa hasta que detecta el caracter de nueva línea '\n' y luego pasa a la siguiente línea. 

## Ver También

Para un estudio más profundo sobre la lectura y escritura de archivos en C++, consulta estas fuentes adicionales.

1. Documentación oficial de CPP: https://en.cppreference.com/w/cpp/io
2. Biblioteca Boost: https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm
3. Guías de programación de archivos: https://www.cplusplus.com/doc/tutorial/files/