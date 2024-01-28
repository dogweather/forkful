---
title:                "Eliminando comillas de una cadena"
date:                  2024-01-26T03:38:07.668373-07:00
model:                 gpt-4-0125-preview
simple_title:         "Eliminando comillas de una cadena"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Eliminar comillas de una cadena significa quitar esos molestos caracteres dobles o simples que encierran nuestro texto (' o "). Los programadores a menudo hacen esto para sanear la entrada, almacenar texto en una base de datos o preparar cadenas para un procesamiento posterior sin el desorden de las comillas.

## Cómo hacerlo:
Aquí hay una forma sencilla de deshacerse de esas comillas en C++:

```cpp
#include <iostream>
#include <algorithm>

std::string remove_quotes(std::string input) {
    input.erase(std::remove(input.begin(), input.end(), '\"'), input.end());
    input.erase(std::remove(input.begin(), input.end(), '\''), input.end());
    return input;
}

int main() {
    std::string original = R"("Hola, 'Mundo'!")";
    std::string sin_comillas = remove_quotes(original);
    std::cout << sin_comillas << std::endl;
    return 0;
}
```

Ejecuta esto, y obtendrás:

```
Hola, Mundo!
```

¡Voilà! Las comillas han desaparecido.

## Análisis Profundo
Las comillas han sido una molestia textual desde el amanecer de la computación. En el pasado, verías a los programadores pasando laboriosamente por cada carácter para filtrar esas comillas. Hoy, tenemos `std::remove` en la Biblioteca de Plantillas Estándar (STL) para hacer el trabajo pesado.

¿Alternativas? ¡Por supuesto! Podrías usar expresiones regulares con `std::regex` para apuntar a las comillas, pero eso es un poco como usar un martillo para romper una nuez: poderoso, pero puede ser excesivo para tareas simples. Para aquellos que favorecen las versiones recientes de C++, podrían experimentar con `std::string_view` para enfoques no modificadores.

En cuanto a la implementación, recuerda que `std::remove` en realidad no elimina elementos del contenedor; reubica los elementos no eliminados hacia adelante y devuelve un iterador más allá del nuevo final del rango. Por eso necesitamos el método `erase` para cortar la cola no deseada.

## Ver También
- Referencia de C++ `std::remove`: [cppreference.com](https://en.cppreference.com/w/cpp/algorithm/remove)
- Más sobre manipulación de `std::string`: [cplusplus.com](http://www.cplusplus.com/reference/string/string/)
