---
title:                "Imprimiendo salida de depuración"
html_title:           "C++: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un programador de C++, probablemente ya sepas que el proceso de depuración puede ser complicado y llevar mucho tiempo. Afortunadamente, imprimir información de depuración en la consola puede ser una herramienta útil para detectar y corregir errores en tu código. 

## Cómo hacerlo

Para imprimir información de depuración en la consola, primero debes incluir la biblioteca de entrada / salida estándar de C++, `iostream`. Luego, puedes utilizar la función `cout` para imprimir la información deseada. Aquí hay un ejemplo básico:

```C++
#include <iostream>

int main(){
    std::cout << "Información de depuración: " << 5 << std::endl;
    return 0;
}
```

Este código imprimirá "Información de depuración: 5" en la consola.

Puedes imprimir cualquier tipo de dato con `cout`, incluyendo variables y expresiones matemáticas. También puedes combinar varias impresiones en una sola línea utilizando el operador `<<`. A continuación se muestra un ejemplo más complejo:

```C++
#include <iostream>

int main(){
    int x = 10;
    int y = 7;
    std::cout << "La suma de " << x << " y " << y << " es " << x+y << std::endl;
    return 0;
}
```

Este código imprimirá "La suma de 10 y 7 es 17" en la consola.

## Profundizando

Además de imprimir información de depuración en la consola, también puedes utilizar la función `cerr` para imprimir mensajes de error en la consola. Esto puede ser útil cuando estás depurando tu programa y necesitas hacer un seguimiento de los errores.

Otra técnica útil es utilizar macros para imprimir información de depuración. Esto te permite activar o desactivar fácilmente la impresión de información de depuración en función de una variable de entorno o una configuración específica.

## Ver también

- [Depuración de programas en C++](https://es.wikipedia.org/wiki/Depuraci%C3%B3n_de_programas_en_C%2B%2B)
- [Cómo utilizar la biblioteca de entrada / salida estándar en C++](https://www.programiz.com/cpp-programming/library-function/cout)
- [Debugging C++ code with Macros](https://www.geeksforgeeks.org/debugging-techniques-in-cpp-macros/)