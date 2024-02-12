---
title:                "Conversión de una cadena de texto a minúsculas"
aliases:
- /es/cpp/converting-a-string-to-lower-case/
date:                  2024-01-20T17:38:09.241169-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversión de una cadena de texto a minúsculas"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Convertir una cadena de texto a minúsculas significa pasar cada letra mayúscula de un string a su equivalente en minúscula. Los programadores hacen esto para unificar datos, facilitar comparaciones y evitar errores por diferencias de mayúsculas/minúsculas.

## Cómo hacerlo:

```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string texto = "¡Hola Mundo!";
    std::transform(texto.begin(), texto.end(), texto.begin(), ::tolower);
    std::cout << texto << std::endl; // Imprime: "¡hola mundo!"
    return 0;
}
```

## Inmersión Profunda

Históricamente, convertir cadenas de texto a minúsculas ha sido fundamental en la normalización de datos y búsquedas insensibles a mayúsculas/minúsculas. En C++, antes de C++11, se podía usar `std::transform` con `::tolower` de `<cctype>`, pero cuidado con los locales! Hoy, `<algorithm>` y lambdas son opciones más seguras.

Alternativas incluyen usar `for` y manipular los caracteres directamente. Existen también otras bibliotecas como Boost que ofrecen soluciones más avanzadas.

En cuanto a implementación, `std::transform` aplica la función pasada (en este caso `::tolower`) a cada elemento del rango dado. `::tolower`, parte de `cctype`, convierte caracteres individuales, respetando la configuración regional (locale).

## Ver También

- Documentación de `std::transform` de cppreference: https://en.cppreference.com/w/cpp/algorithm/transform
- Documentación de `::tolower` de cppreference: https://en.cppreference.com/w/cpp/string/byte/tolower
- Guía de Boost String Algorithms: https://www.boost.org/doc/libs/1_75_0/doc/html/string_algo.html
