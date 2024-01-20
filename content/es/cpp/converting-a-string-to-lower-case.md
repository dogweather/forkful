---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

Título: Convertir una cadena a minúsculas en C++

## ¿Qué & Por qué?
Convertir una cadena de texto a minúsculas en C++ implica cambiar todas las letras mayúsculas de la cadena a minúsculas. Los programadores frecuentemente realizan esta operación para estandarizar los datos de entrada y facilitar las operaciones de comparación.

## Cómo hacerlo:
Aquí se encuentra un ejemplo clásico de código en C++ para convertir una cadena a minúsculas:

```C++
#include <algorithm>
#include <string>
#include <iostream>

int main() {
    std::string miCadena = "HOLA MUNDO";
    std::transform(miCadena.begin(), miCadena.end(), miCadena.begin(), ::tolower);
    std::cout << miCadena << std::endl;
    return 0;
}
```

Este código imprimirá:

```C++
hola mundo
```

## Profundización:
Convertir una cadena a minúsculas es una operación común en muchos lenguajes de programación, y en C++ se ha vuelto aún más sencillo con la función estándar `std::transform`. Sin embargo, existen alternativas.

Por ejemplo, puedes usar un bucle simple para recorrer cada carácter de la cadena y convertirlo a minúsculas utilizando la función tolower de la biblioteca cctype. Aunque esta forma es un poco más larga, te da más control sobre el proceso.

El detalle de implementación más importante a tener en cuenta al convertir cadenas a minúsculas en C++ es que las funciones estándar tratan únicamente con caracteres ASCII. Si estás trabajando con caracteres no ASCII, como los de la alfabetización española (por ejemplo, la ñ), tendrás que buscar una solución personalizada o una biblioteca que soporte estas operaciones.

## Ver también:
Aquí tienes algunos enlaces útiles para profundizar en el tema:

1. Documentación oficial de C++: https://en.cppreference.com/w/cpp/string/basic_string
2. Artículo detallado sobre cómo convertir cadenas a minúsculas en CPP: https://www.educba.com/cpp-tolower/
3. Información adicional acerca de std::transform: https://www.cplusplus.com/reference/algorithm/transform/