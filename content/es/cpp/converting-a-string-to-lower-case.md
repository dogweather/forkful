---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "C++: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Convertir una cadena de texto a minúsculas es un proceso común en la programación en C++. Consiste en transformar todas las letras mayúsculas de una cadena a su equivalente en minúsculas. Los programadores lo hacen para facilitar la comparación de cadenas y simplificar el manejo de datos.

## ¿Cómo hacerlo?

El siguiente código muestra cómo convertir una cadena a minúsculas en C++:

```
#include <iostream>
#include <cstring>
using namespace std;

int main() {
    // Definir una cadena de texto
    char palabra[] = "PROGRAMACIÓN EN C++";

    // Utilizar la función strlwr de la librería cstring
    strlwr(palabra);

    // Imprimir el resultado
    cout << palabra << endl;

    return 0;
}

```

La salida de este código será "programación en c++", con todas las letras en minúsculas.

## Detalles técnicos

El proceso de convertir una cadena a minúsculas es conocido como "lowercasing" en inglés y "minúsculas" en español. En la versión 4.2 del estándar de C++, se introdujo la función strlwr en la librería cstring para facilitar esta tarea.

Una alternativa a utilizar la función strlwr es recorrer cada letra de la cadena y utilizar la función tolower para convertirla individualmente. Sin embargo, esto sería más tedioso y menos eficiente en términos de tiempo de ejecución.

## Enlaces de interés

- Referencia de la función strlwr en la [documentación de C++](https://www.cplusplus.com/reference/cstring/strlwr/).
- Más información sobre el proceso de "lowercasing" en [este artículo de Wikipedia](https://en.wikipedia.org/wiki/Lowercase).