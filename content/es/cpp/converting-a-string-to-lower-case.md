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

## Por qué
Hay momentos en los que necesitas manipular cadenas de texto en C++, ya sea para realizar búsquedas, comparaciones o simplemente mejorar la usabilidad de tu programa. Convertir una cadena de texto a minúsculas te permite optimizar estas tareas de manera más eficiente.

## Cómo hacerlo
Para convertir una cadena de texto a minúsculas en C++, puedes utilizar la función `tolower()` de la librería `ctype`. Aquí hay un ejemplo de cómo utilizarla:

```C++
#include <iostream>
#include <ctype.h>
using namespace std;

int main() {
    string texto = "Ejemplo de TEXTO en MAYÚSCULAS";
    
    // Convertir el texto a minúsculas utilizando un bucle for
    for (int i = 0; i < texto.length(); i++) {
        texto[i] = tolower(texto[i]);
    }

    cout << "El texto en minúsculas es: " << texto << endl;
    return 0;
}
```

**Salida:**

`El texto en minúsculas es: ejemplo de texto en mayúsculas`

También puedes usar la función `transform()` de la librería `algorithm` para convertir una cadena entera a minúsculas, sin necesidad de utilizar un bucle `for`:

```C++
#include <iostream>
#include <algorithm> // Para la función transform
#include <cctype> // Para la función toupper
using namespace std;

int main() {
    string texto = "Ejemplo de TEXTO en MAYÚSCULAS";
    
    // Convertir el texto a minúsculas utilizando 
    // la función transform y la función toupper
    transform(texto.begin(), texto.end(), texto.begin(), ::tolower);

    cout << "El texto en minúsculas es: " << texto << endl;
    return 0;
}
```

**Salida:**

`El texto en minúsculas es: ejemplo de texto en mayúsculas`

## Profundizando
La función `tolower()` de la librería `ctype` toma como parámetro un único carácter y devuelve su equivalente en minúsculas. Por lo tanto, es necesario utilizar un bucle `for` para recorrer toda la cadena de texto y aplicar la función a cada uno de los caracteres.

Por otro lado, la función `transform()` de la librería `algorithm` toma tres argumentos: el inicio del rango, el final del rango y una función que define qué transformación se debe aplicar a cada elemento del rango. En este caso, utilizamos la función `::tolower` para aplicar la transformación a cada carácter de la cadena de texto.

## Ver también
- [Documentación de la función tolower()](https://www.cplusplus.com/reference/cctype/tolower/)
- [Documentación de la función transform](https://www.cplusplus.com/reference/algorithm/transform/)