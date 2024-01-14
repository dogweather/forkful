---
title:    "C++: Escribir un archivo de texto."
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué escribir un archivo de texto en C++

Escribir un archivo de texto en C++ puede ser una tarea útil y necesaria en la programación. A través de la escritura de un archivo de texto, puedes almacenar y guardar información que puede ser utilizada posteriormente en tu programa. También puedes crear archivos de texto para usuarios finales, como configuraciones o registros.

## Cómo escribir un archivo de texto en C++

Para escribir un archivo de texto en C++, necesitarás seguir estos pasos:

1. Inicializar una instancia de `ofstream` para crear y escribir en un archivo.
2. Abrir el archivo utilizando el método `open()` para especificar nombre y ubicación.
3. Escribir en el archivo utilizando el operador `<<`, similar a como lo haces con `cout` para imprimir en la pantalla.
4. Cerrar el archivo utilizando el método `close()` una vez que hayas terminado de escribir en él.

A continuación, se muestra un ejemplo de código en C++ que escribe un archivo de texto llamado "saludo.txt" con el mensaje "¡Hola, mundo!":

```c++
#include <iostream>
#include <fstream>  // para ofstream

using namespace std;

int main()
{
    ofstream archivo;  // inicializar instancia de ofstream
    archivo.open("saludo.txt");  // abrir archivo
    archivo << "¡Hola, mundo!";  // escribir en el archivo
    archivo.close();  // cerrar archivo
    return 0;
}
```

El archivo "saludo.txt" ahora debería contener el mensaje "¡Hola, mundo!".

## Profundizando en la escritura de archivos de texto

Además de escribir en un archivo de texto, también puedes hacer lo siguiente:

- Crear diferentes tipos de archivos de texto, como CSV o XML.
- Escribir y leer estructuras de datos complejas en archivos de texto utilizando bucles y operadores de fluxión.
- Manejar errores al escribir un archivo utilizando la sentencia `if-else` y lanzar excepciones.

La escritura de archivos de texto también es útil en la creación de programas con interfaces de usuario, ya que puedes guardar configuraciones o registros para que los usuarios finales puedan acceder y modificar.

## Ver también

- [Tutorial de escritura de archivos de texto en C++](https://www.programiz.com/cpp-programming/writing-file)
- [Más ejemplos de escritura de archivos de texto en C++](https://www.geeksforgeeks.org/writing-text-file-c/)
- [Documentación de la clase `ofstream` en C++](http://www.cplusplus.com/reference/fstream/ofstream/)