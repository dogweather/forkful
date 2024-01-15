---
title:                "Encontrando la longitud de una cadena"
html_title:           "C++: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Por qué 
En programación, a menudo tenemos que trabajar con cadenas de texto. Puede ser útil saber la longitud exacta de una cadena para realizar ciertas operaciones, como la búsqueda o la manipulación de datos. En este artículo, aprenderemos cómo encontrar la longitud de una cadena en C ++. 

## Cómo hacerlo 
Para encontrar la longitud de una cadena en C ++, podemos usar la función integrada `strlen()`. Esta función toma una cadena como argumento y devuelve un número entero que representa la longitud de la cadena. Veamos un ejemplo: 

```C++
#include <iostream>
#include <cstring>

using namespace std;

int main() {
    string myString = "Hola mundo!";
    int length = strlen(myString.c_str()); // convertimos la cadena en un array de caracteres con c_str()
    cout << "La longitud de la cadena es: " << length << endl;
    return 0;
}
```
Output: `La longitud de la cadena es: 11`

Podemos ver que la longitud es 11, ya que la función `strlen()` cuenta cada carácter en la cadena, incluyendo espacios y signos de puntuación. 

## Profundizando 
Ahora que sabemos cómo encontrar la longitud de una cadena en C ++, es importante entender cómo funciona la función `strlen()`. 

En primer lugar, esta función se encuentra en la biblioteca `cstring`, por lo que debemos incluirla en nuestro programa para poder usarla. 

En segundo lugar, debemos tener en cuenta que la función cuenta el número de caracteres hasta que encuentra un carácter nulo (`\0`), que marca el final de la cadena. Por lo tanto, es importante asegurar que nuestras cadenas tengan un carácter nulo al final para obtener los resultados correctos. 

## Ver también 
- [Documentación de la función `strlen()` en C++](https://www.cplusplus.com/reference/cstring/strlen/)
- [Ejemplos de uso de la función `strlen()`](https://www.geeksforgeeks.org/strlen-function-in-cpp/)
- [Más información sobre cadenas de texto en C ++](https://www.studytonight.com/cpp/string-basics.php)