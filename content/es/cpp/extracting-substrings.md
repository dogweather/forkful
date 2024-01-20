---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Extraer subcadenas es un proceso por el cual obtenemos una porción de una cadena existente. Lo hacemos porque a menudo hay situaciones en las que solo requerimos una parte de la cadena para realizar operaciones específicas.

## Como hacerlo:
Aquí está el código más básico para extraer subcadenas en C++ utilizando el método `substr`.

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
   string mensaje = "Hola programadores!";
   string subcadena = mensaje.substr(5, 13);

   cout << "Subcadena: " << subcadena << endl;
   return 0;
}
```

Salida: 

```
Subcadena: programadores!
```

La sintaxis para el método `substr` en C++ es `string.substr(start, length)`, donde `start` es el lugar para comenzar la extracción y `length` es la cantidad de caracteres a extraer.

## Profundizando
1. Contexto histórico: El método `substr` ha estado disponible desde los primeros estándares de C++. Su utilidad para trabajar con cadenas le ha valido un lugar en todas las versiones posteriores de C++.

2. Alternativas: También puedes usar operadores de índice o iteradores para extraer subcadenas, pero el método `substr` es más legible y fácil de usar.

3. Detalles de implementación: En C++, las cadenas son objetos de la clase `std::string`. La función `substr` es un método de la clase `std::string`. Genera internamente una nueva cadena que contiene los caracteres de la cadena original desde el índice de inicio hasta el índice de fin.

## Vea También
1. Documentación oficial sobre std::string::substr — https://en.cppreference.com/w/cpp/string/basic_string/substr
2. Trabajar con cadenas en C++ — https://www.w3schools.com/cpp/cpp_strings.asp
3. Más sobre la clase std::string — https://www.cplusplus.com/reference/string/string/