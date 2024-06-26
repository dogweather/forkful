---
date: 2024-01-20 17:45:17.840882-07:00
description: "C\xF3mo hacerlo: Hist\xF3ricamente, extraer subcadenas ha sido esencial\
  \ desde los primeros d\xEDas de la programaci\xF3n. C++ lo ha simplificado con la\
  \ clase\u2026"
lastmod: '2024-04-05T22:51:13.086709-06:00'
model: gpt-4-1106-preview
summary: "Hist\xF3ricamente, extraer subcadenas ha sido esencial desde los primeros\
  \ d\xEDas de la programaci\xF3n."
title: "Extracci\xF3n de subcadenas"
weight: 6
---

## Cómo hacerlo:
```cpp
#include <iostream>
#include <string>

int main() {
    std::string texto = "Hola, mundo de la programación!";
    std::string subcadena = texto.substr(7, 5); // empieza en índice 7, longitud 5

    std::cout << "Subcadena extraída: " << subcadena << std::endl; // 'mundo'

    return 0;
}
```
Salida:
```
Subcadena extraída: mundo
```

## Análisis Profundo:
Históricamente, extraer subcadenas ha sido esencial desde los primeros días de la programación. C++ lo ha simplificado con la clase `std::string` y su método `substr()`. Alternativas incluyen el uso de punteros y funciones como `std::string::copy()` y operaciones de los iteradores de la STL. Detalles de implementación que hay que tener en cuenta incluyen el manejo de excepciones como `std::out_of_range` si los índices exceden los límites de la cadena.

## Vea También:
- Documentación de `std::string::substr`: https://en.cppreference.com/w/cpp/string/basic_string/substr
- Tutorial de C++ Strings: https://www.cplusplus.com/reference/string/string/
- Referencia de la biblioteca de cadenas de C++ (STL): https://www.cplusplus.com/reference/string/
