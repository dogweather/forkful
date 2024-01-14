---
title:    "C++: Convirtiendo una cadena a minúsculas"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por qué

Existen muchas situaciones en las que es necesario convertir una cadena de texto a minúsculas en un programa de C++. Por ejemplo, si queremos comparar dos cadenas de texto sin importar si están en mayúsculas o minúsculas, necesitamos asegurarnos de que ambas estén en el mismo formato. También puede ser útil en situaciones en las que se desee cambiar el formato de una cadena de texto para que sea más fácil de procesar.

## Cómo hacerlo

Convertir una cadena de texto a minúsculas en C++ es muy sencillo. Utilizando la función `tolower` de la biblioteca `cctype`, podemos convertir cada carácter de la cadena a su equivalente en minúscula. Aquí hay un ejemplo de cómo hacerlo:

```C++
#include <iostream>
#include <cctype>

using namespace std;

int main() {
    string texto = "Esto es una CADENA de TEXTO";

    for (char& c : texto) {
        c = tolower(c);
    }

    cout << texto << endl;

    return 0;
}
```

Este ejemplo utiliza un bucle `for` para recorrer cada caracter de la cadena de texto y luego, utilizando la función `tolower`, convertirlo a minúscula. El resultado de este programa sería: `esto es una cadena de texto`.

## Profundizando

La función `tolower` pertenece a la biblioteca `cctype` la cual contiene un conjunto de funciones para la manipulación de caracteres en C++. Esta función recibe como parámetro un caracter de tipo `int` y devuelve su equivalente en minúscula. Es importante destacar que esta función solamente transforma caracteres que son letras, por lo que cualquier otro caracter no será modificado.

Por otro lado, es necesario mencionar que esta función también tiene un homólogo para convertir caracteres a mayúsculas, llamado `toupper`. Ambas funciones son muy útiles en la manipulación de cadenas de texto.

## Ver también

- [Guía completa de la biblioteca cctype en C++](https://www.cplusplus.com/reference/cctype/)
- [Cómo comparar cadenas de texto sin tener en cuenta mayúsculas o minúsculas en C++](https://www.geeksforgeeks.org/case-conversion-in-string-in-c/)
- [Ejemplos de uso de la función tolower en C++](https://www.guru99.com/cpp-conversion-tolower-toupper-islower.html)