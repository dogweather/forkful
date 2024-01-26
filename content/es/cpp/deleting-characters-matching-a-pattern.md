---
title:                "Eliminando caracteres que coinciden con un patrón"
date:                  2024-01-20T17:41:42.671248-07:00
model:                 gpt-4-1106-preview
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Qué y por qué?
Eliminar caracteres que coinciden con un patrón es sencillamente quitar ciertos caracteres de una cadena de texto basándose en reglas específicas o patrones. Los programadores hacen esto para limpiar datos, validar entradas, o preparar texto para procesos posteriores como análisis o almacenamiento.

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
