---
title:                "C++: Escribiendo en el estándar de error"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué escribir a la salida de error estándar

Es común que al escribir un programa en C++, nos encontremos con errores del compilador o con situaciones en las que necesitamos mostrar mensajes de error al usuario. A veces, estos mensajes se pueden mostrar en la salida estándar, pero en ocasiones es más importante mostrarlos en la salida de error estándar. Esta es una forma de distinguir claramente el mensaje de error de la información importante del programa.

## Cómo escribir a la salida de error estándar

Para escribir a la salida de error estándar en C++, se utiliza la función "cerr" de la biblioteca estándar. Esta función se usa de la misma manera que la función "cout", pero en lugar de mostrar el mensaje en la salida estándar, lo muestra en la salida de error estándar. Aquí hay un ejemplo de cómo usarlo:

```C++
#include <iostream>
using namespace std;

int main() {
    int x = 10;
    if (x % 2 == 0) {
        cerr << "El número es par." << endl;
    } else {
        cerr << "El número es impar." << endl;
    }
    return 0;
}
```

El resultado de este programa sería "El número es par." Si cambiamos el valor de "x" a 11, el mensaje de error sería "El número es impar."

## Profundizando en la escritura a la salida de error estándar

Como mencionamos antes, la principal diferencia entre "cerr" y "cout" es que "cerr" es la salida de error estándar, mientras que "cout" es la salida estándar. Esto significa que los mensajes de "cerr" se muestran en una pantalla diferente que los de "cout". Por lo general, la salida de error estándar es la consola en la que se está ejecutando el programa, pero también puede ser redirigida a un archivo si es necesario.

También es importante tener en cuenta que los mensajes de "cerr" no se muestran en diferentes colores ni se guardan en un archivo de registro como los mensajes de "cout". Sin embargo, seguimos recomendando su uso para mostrar mensajes de error y garantizar que estos se destaquen claramente de la salida estándar.

## Ver también

- [Introducción a la programación en C++](https://www.freecodecamp.org/espanol/news/introduccion-a-c-plus-plus/)
- [Documentación de la función cerr en cplusplus.com](https://www.cplusplus.com/reference/iostream/cerr/)