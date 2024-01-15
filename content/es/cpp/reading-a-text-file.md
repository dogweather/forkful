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

## Por qué
Si estás aprendiendo a programar en C++ o si ya tienes experiencia, es importante entender cómo leer un archivo de texto. Este conocimiento te permitirá manejar y procesar grandes cantidades de datos almacenados en un archivo de texto.

## Cómo hacerlo
Para leer un archivo de texto en C++, necesitarás seguir los siguientes pasos:

1. Incluir la biblioteca fstream en tu programa: ```C++
#include <fstream>
```
2. Crear un objeto ifstream para manejar el archivo de texto: ```C++
ifstream archivo;
```
3. Abrir el archivo de texto en modo lectura (ios::in): ```C++
archivo.open("archivo.txt", ios::in);
```
4. Verificar que el archivo se haya abierto correctamente: ```C++
if (!archivo) {
  cout << "Error al abrir el archivo.";
  exit(1);
}
```
5. Leer el contenido del archivo línea por línea usando un ciclo while: ```C++
string linea;
while (getline(archivo, linea)) {
  // Procesar la línea de texto
}
```
6. Cerrar el archivo una vez que hayas terminado de leerlo: ```C++
archivo.close();
```

A continuación, se muestra un ejemplo completo de cómo leer un archivo de texto en C++:

```C++
#include <iostream>
#include <fstream>
using namespace std;

int main() {
  ifstream archivo;
  string linea;
  
  archivo.open("archivo.txt", ios::in);
  if (!archivo) {
    cout << "Error al abrir el archivo.";
    exit(1);
  }

  while (getline(archivo, linea)) {
    cout << linea << endl; // Imprimir la línea de texto
  }
  archivo.close();
  return 0;
}
```

El código anterior imprimirá cada línea del archivo de texto en la consola.

## Profundizando
Al leer un archivo de texto en C++, es importante entender cómo funciona el proceso detrás de escena. Cuando abres un archivo en modo lectura, el sistema operativo asigna un búfer para almacenar temporalmente los datos del archivo. En cada iteración del ciclo while, se lee una cantidad específica de datos del archivo en el búfer y luego se procesan, lo que reduce la cantidad de acceso al disco y hace que el proceso sea más eficiente.

También es importante tener en cuenta que el objeto ifstream se puede utilizar para leer diferentes tipos de datos, como enteros, flotantes, caracteres, entre otros. Simplemente necesitas utilizar los métodos adecuados, como ``` archivo >> variable; ``` para leer un valor de una línea específica del archivo.

## Ver también
- [Documentación oficial de C++ sobre lectura de archivos](https://es.cppreference.com/w/cpp/io/basic_ifstream)
- [Tutorial sobre lectura de archivos en C++](https://www.programiz.com/cpp-programming/file-handling)