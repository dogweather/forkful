---
title:                "Interpolando una cadena de texto"
html_title:           "Haskell: Interpolando una cadena de texto"
simple_title:         "Interpolando una cadena de texto"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Cadena de Interpolación en C++: Su 'Qué' y 'Por Qué'  

Interpolar una cadena puede ser tan simple como insertar valores en una cadena de texto. Los programadores hacen esto para construir cadenas de forma dinámica, lo cual permite que los datos sean expresados de una forma más natural y legible.

## Cómo hacerlo: 

En C++, interpolación de cadena puede ser lograda utilizando la biblioteca `fmt`, que es oficialmente parte de C++20. A continuación tienes un ejemplo.

```C++
#include <fmt/core.h>

int main() {
    int edad = 25;
    std::string nombre = "Carlos";
    fmt::print("Hola, {}. Tienes {} años.\n", nombre, edad);
    return 0;
}
```

La salida será:
```
Hola, Carlos. Tienes 25 años.
```  

## Profundizando:  

A pesar de que C++ ahora soporta la interpolación de cadenas, históricamente no siempre fue así. Antes de C++20, tenías que utilizar `printf` o una secuencia complicada de operaciones de inserción.

Hay alternativas a `print` de `fmt`, como el clásico `printf` de C o `stringstream`, pero `fmt` es más eficiente y seguro contra tipos.

La interpolación de cadenas en C++ mediante `fmt` es implementada a través de la expansión de plantillas en tiempo de compilación, lo cual es más eficiente en tiempo de ejecución en comparación con el procesamiento en tiempo de ejecución propio de otros lenguajes.

## Ver también:  

Aquí están algunos recursos útiles para la interpolación de cadenas en C++:

- Documentación oficial de `fmt`: https://fmt.dev/latest/index.html
- Tutorial en línea sobre C++20: https://learncpp.com/
- `printf` vs `fmt::print` : https://www.zverovich.net/2020/06/13/fmt-vs-printf.html