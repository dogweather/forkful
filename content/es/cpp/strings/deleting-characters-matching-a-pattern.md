---
date: 2024-01-20 17:41:42.671248-07:00
description: "How to: En los viejos tiempos, los programadores ten\xEDan que recorrer\
  \ una cadena car\xE1cter por car\xE1cter y manejar manualmente el proceso de eliminaci\xF3\
  n.\u2026"
lastmod: '2024-04-05T21:54:00.708389-06:00'
model: gpt-4-1106-preview
summary: "En los viejos tiempos, los programadores ten\xEDan que recorrer una cadena\
  \ car\xE1cter por car\xE1cter y manejar manualmente el proceso de eliminaci\xF3\
  n."
title: "Eliminando caracteres que coinciden con un patr\xF3n"
weight: 5
---

## How to:
```C++
#include <iostream>
#include <regex>
#include <string>

int main() {
    std::string texto = "H0l4 Mund0! C++ m0l4.";
    std::regex patron("[0-9]"); // Define el patrón para buscar dígitos.

    // Reemplaza los caracteres que coinciden con el patrón por una cadena vacía.
    std::string resultado = std::regex_replace(texto, patron, "");
    
    std::cout << resultado << std::endl; // Salida: "Hlá Mund! C++ mólá."
    
    return 0;
}
```

## Deep Dive
En los viejos tiempos, los programadores tenían que recorrer una cadena carácter por carácter y manejar manualmente el proceso de eliminación. Desde C++11, la biblioteca estándar proporciona `<regex>`, una forma poderosa y flexible para manejar expresiones regulares. Existen alternativas como usar el método `erase` o `remove_if` junto con lambdas para una eliminación más específica y controlada sin expresiones regulares. La implementación detrás de `std::regex_replace` es compleja; optimiza la búsqueda y reemplazo, pero tiene un costo de rendimiento mayor comparado con métodos más directos para escenarios sencillos.

## See Also
- Documentación oficial de C++ para `<regex>`: https://en.cppreference.com/w/cpp/regex
- Tutorial interactivo de expresiones regulares: https://regexr.com/
- Guía sobre lambdas y `remove_if`: https://en.cppreference.com/w/cpp/algorithm/remove
