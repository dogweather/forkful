---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elixir: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Eliminar caracteres de coincidencia es buscar una secuencia o patrón de caracteres en un texto, y después, quitar estas instancias. Los programadores lo hacen para limpiar o normalizar textos, por ejemplo, al eliminar caracteres no deseados.

## Cómo hacerlo:

Vamos a usar la biblioteca `algorithm` y el método `remove_if` para conseguirlo en C++. 

```C++
#include <algorithm>
#include <cctype>

std::string str = "¡Hola, Mundo!";

str.erase(std::remove_if(str.begin(), str.end(), [](unsigned char c) { return std::ispunct(c); }), str.end());

std::cout << str << std::endl; // Imprime: "Hola Mundo"
```

En este código, `remove_if` mapea por cada carácter en la variable `str`, y elimina los caracteres que coinciden con el patrón, en este caso, la puntuación. 

## Más detalles

Historicamente, antes de las bibliotecas estándares actuales en C++, los programadores tenían que escribir estas funciones manualmente, lo que podía resultar en errores difíciles de detectar.

Existen otras maneras de eliminar caracteres que coinciden con un patrón; por ejemplo, utilizando expresiones regulares, aunque estas pueden resultar más complicadas y consumir más recursos.

En términos de implementación, `remove_if` recorre secuencialmente la cadena de texto para determinar qué elementos eliminar. La función `erase` se emplea para modificar efectivamente la cadena una vez que `remove_if` ha identificado los caracteres a eliminar.

## Ver también

- Documentación oficial de C++ `remove_if`: http://www.cplusplus.com/reference/algorithm/remove_if/ 
- `regex_replace` para el uso de expresiones regulares: http://www.cplusplus.com/reference/regex/regex_replace/