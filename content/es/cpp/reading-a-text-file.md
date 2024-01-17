---
title:                "Leyendo un archivo de texto"
html_title:           "C++: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¡Qué es y por qué!

Leer un archivo de texto en programación es una forma de acceder a la información almacenada en un archivo que contiene texto plano. Los programadores a menudo lo hacen para leer y procesar datos estructurados contenidos en un archivo de texto, como configuraciones, bases de datos simples o registros de actividad.

## Cómo hacerlo:

```C++
#include <iostream> //biblioteca estándar para entrada/salida
#include <fstream> //biblioteca para manejar archivos

using namespace std;

int main() {
    ifstream archivo; //crea un objeto para leer archivos
    archivo.open("archivo.txt"); //abre el archivo de texto

    //itera sobre cada línea del archivo de texto
    string linea;
    while (getline(archivo, linea)) { 
        //haz algo con la línea, como imprimir en la consola
        cout << linea << endl;
    }

    archivo.close(); //cierra el archivo

    return 0;
}
```

**Salida de ejemplo:**
```
Hola, mundo!
Este es un archivo de texto.
Contiene varias líneas.
```

## Profundizando:

Antes de que surgieran las bases de datos, los archivos de texto eran una forma común de almacenar y procesar información en una computadora. Aunque todavía se utilizan hoy en día para ciertos propósitos, las bases de datos han ganado popularidad debido a su capacidad para manejar grandes cantidades de datos de forma más eficiente.

Hay varias formas de leer archivos de texto en C++, incluyendo la función `fgets()` y la librería [Boost](https://www.boost.org/doc/libs/1_77_0/libs/iostreams/doc/coding_guide.html#static_vs_dynamic). Sin embargo, la forma más sencilla es utilizar la librería estándar `<fstream>`.

Para leer un archivo de texto, la computadora primero debe abrir y leer el archivo, luego procesar los datos y finalmente cerrar el archivo antes de devolver el control al usuario. Es importante seguir este flujo para evitar problemas de lectura o escritura en el archivo.

## Ver también:

- [Leer y escribir archivos de texto en C++](https://es.cppreference.com/w/cpp/io/basic_filebuf)
- [Introducción a la librería Boost para manipulación de archivos](https://www.boost.org/doc/libs/1_77_0/libs/iostreams/doc/coding_guide.html#introduction)