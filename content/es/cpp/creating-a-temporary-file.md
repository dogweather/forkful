---
title:                "C++: Creando un archivo temporal"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Por qué crear un archivo temporal?

Crear un archivo temporal es una práctica común en la programación para almacenar datos temporales o para realizar operaciones específicas que requieren un archivo temporal como intermediario. Puede ser útil para ahorrar espacio en el sistema o para evitar sobrecargar el disco duro con archivos que ya no son necesarios.

## ¿Cómo crear un archivo temporal en C ++?

Para crear un archivo temporal en C ++, podemos seguir estos pasos:

```C++
#include <fstream> // librería para trabajar con archivos
#include <cstdlib> // librería para generar números aleatorios
using namespace std;

int main() {
  // generamos un nombre de archivo único utilizando un número aleatorio
  int randNum = rand() % 10000; // generamos un número aleatorio entre 0 y 9999
  string fileName = "temp" + to_string(randNum) + ".temp"; // concatenamos el nombre con el número
  // abrimos el archivo en modo escritura
  ofstream temp(fileName);
  // escribimos datos en el archivo
  temp << "¡Hola desde mi archivo temporal!";
  // cerramos el archivo
  temp.close();

  return 0;
}
```

Una vez que ejecutamos el programa, se creará un archivo temporal con un nombre único, en este caso "temp6759.temp". Al abrir el archivo, veremos que contiene el mensaje "¡Hola desde mi archivo temporal!".

## Profundizando en la creación de archivos temporales

Existen varias formas de crear archivos temporales en C ++, pero también es importante destacar que no es una práctica recomendable para almacenar datos sensibles o importantes, ya que estos archivos pueden ser eliminados por el sistema en cualquier momento. Además, es importante asegurarse de que el archivo temporal se elimine correctamente después de su uso para no dejar residuos innecesarios en el sistema.

## Ver también

- [Cómo abrir y leer un archivo en C++](https://www.programiz.com/cpp-programming/file-handling)
- [Documentación oficial de la librería `<fstream>` en C++](https://www.cplusplus.com/reference/fstream/)
- [Cómo trabajar con archivos en C++](https://www.geeksforgeeks.org/file-handling-c-classes/)