---
title:    "C++: Leyendo un archivo de texto."
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por qué

Si estás interesado en convertirte en un programador de C++, entonces necesitas aprender a trabajar con archivos de texto. Leer un archivo de texto es una habilidad básica y esencial en la programación, ya que te permitirá acceder y manipular información almacenada en un archivo de texto.

## Cómo hacerlo

Para leer un archivo de texto en C++, debes seguir los siguientes pasos:

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {
    
    // Se define una variable de tipo ifstream 
    // para abrir un archivo de texto
    ifstream archivo("miarchivo.txt");
    
    // Se comprueba si el archivo se abrió correctamente
    if (archivo.is_open())
    {
        // Se declara una variable para almacenar
        // cada línea del archivo
        string linea;
        
        // Se usa un bucle while para leer el archivo
        while (getline(archivo, linea))
        {
            // Se imprime la línea en la consola
            cout << linea << endl;
        }
        
        // Se cierra el archivo
        archivo.close();
    }
    else
    {
        // En caso de que el archivo no se pueda abrir,
        // se imprime un mensaje de error
        cout << "No se pudo abrir el archivo" << endl;
    }
    
    return 0;
}
```

### Ejemplo de output

Supongamos que tenemos un archivo de texto llamado "miarchivo.txt" con el siguiente contenido:

```
Hola, este es un archivo de texto.
Esto es una línea.
Y esta es otra línea.
```

Al ejecutar el código anterior, el output sería:

```
Hola, este es un archivo de texto.
Esto es una línea.
Y esta es otra línea.
```

## Profundizando

En el ejemplo anterior, utilizamos la función `getline()` para leer cada línea del archivo. Esta función lee una línea completa del archivo y la almacena en una variable de tipo `string`. También es posible leer un archivo caracter por caracter utilizando la función `get()`.

Otra cosa importante a tener en cuenta es que, al utilizar la función `getline()`, se lee una línea completa del archivo, incluyendo el carácter de nueva línea (`\n`). Si no se desea incluir este carácter en la variable, se puede utilizar la función `ignore()` para ignorarlo.

### Ejemplo de uso de la función `ignore()`

```C++
// Se declara una variable para almacenar cada línea del archivo
string linea;

// Se usa un bucle while para leer el archivo
while (getline(archivo, linea))
{
    // Se imprime la línea en la consola
    cout << linea << endl;
    
    // Se ignora el carácter de nueva línea
    archivo.ignore();
}
```

Además, C++ también proporciona la clase `stringstream` que permite leer diferentes tipos de datos de una línea de texto. Esto resulta muy útil si el archivo contiene datos de diferentes tipos.

## Ver también

- [Documentación oficial de C++ sobre archivos de entrada y salida](https://en.cppreference.com/w/cpp/io)
- [Tutorial de lectura y escritura de archivos en C++](http://www.cplusplus.com/doc/tutorial/files/)