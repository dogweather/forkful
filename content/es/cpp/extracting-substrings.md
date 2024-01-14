---
title:    "C++: Extracción de subcadenas"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Por qué extraer subcadenas en programación?

Extraer subcadenas es una técnica importante en la programación que permite a los desarrolladores manipular y trabajar con cadenas de texto de manera eficiente. Puede ser útil en situaciones como la validación de entradas de usuario o la búsqueda de palabras clave en un texto.

## Cómo extraer subcadenas en C++

Para extraer una subcadena de una cadena en C++, se puede utilizar la función `substr()` de la clase `string`. Esta función toma dos parámetros: la posición inicial de la subcadena y la longitud deseada.

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    string texto = "Hola mundo";
    string subcadena = texto.substr(5, 5); // subcadena es "mundo"
    cout << subcadena << endl; // Output: mundo
    return 0;
}
```

También se puede usar la función `find()` para encontrar la posición de una subcadena específica dentro de una cadena. Luego, esta posición se puede utilizar como parámetro en la función `substr()` para extraer la subcadena deseada.

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    string texto = "La casa es roja";
    int posicion = texto.find("casa"); // posición es 3
    string subcadena = texto.substr(posicion, 4); // subcadena es "casa"
    cout << subcadena << endl; // Output: casa
    return 0;
}
```

## Explicación más detallada sobre la extracción de subcadenas

La función `substr()` trabaja con índices de posición basados en cero, lo que significa que la primera posición de una cadena es 0 en lugar de 1. Además, si se omite el segundo parámetro (longitud), la función devolverá la subcadena que comienza en la posición dada y continúa hasta el final de la cadena original.

Por lo tanto, al utilizar `substr(5, 5)` en el primer ejemplo, se está indicando que se deseana extraer una subcadena que comienza en la posición 5 y tiene una longitud de 5 caracteres. Esto es útil cuando se conoce la posición y la longitud exactas de la subcadena que se desea extraer.

La función `find()`, por otro lado, devuelve la primera aparición de una subcadena dentro de otra cadena. Si la subcadena no se encuentra, devuelve `-1`. Esta función puede ser útil cuando se desea extraer una subcadena desconocida de una cadena más larga.

## Vea también

- Documentación sobre la función `substr()` de la clase `string`: https://www.cplusplus.com/reference/string/string/substr/
- Documentación sobre la función `find()` de la clase `string`: https://www.cplusplus.com/reference/string/string/find/