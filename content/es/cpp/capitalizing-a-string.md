---
title:                "Convertir una cadena en mayúsculas"
html_title:           "C++: Convertir una cadena en mayúsculas"
simple_title:         "Convertir una cadena en mayúsculas"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué se hace?

Capitalizar una cadena significa convertir su primera letra en mayúscula y el resto en minúsculas. Los programadores suelen hacer esto para mejorar la legibilidad y claridad del código.

## Cómo hacerlo:
```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    // Definir una cadena
    string cadena = "esta es una cadena";

    // Mostrar la cadena original
    cout << "Original: " << cadena << endl;

    // Convertir la cadena a mayúscula
    for(int i=0; i<cadena.length(); i++) {
        cadena[i] = toupper(cadena[i]);
    }

    // Mostrar la cadena capitalizada
    cout << "Capitalizada: " << cadena << endl;

    return 0;
}
```
Resultado:
```
Original: esta es una cadena
Capitalizada: ESTA ES UNA CADENA
```

## Profundizando:
En el pasado, cuando los programadores usaban sistemas operativos con una interfaz de línea de comandos, el uso de mayúsculas y minúsculas era importante ya que los comandos y nombres de archivo distinguen entre mayúsculas y minúsculas. Sin embargo, en la actualidad, en la mayoría de los sistemas operativos modernos esto ya no es un problema.
Existen varias formas de capitalizar una cadena, como usar una función especial o escribiendo un algoritmo propio.
La implementación de la función capitalize() puede variar dependiendo del lenguaje de programación utilizado, pero su lógica básica siempre será la misma.

## Véase también:
- [Función toupper en C++](https://www.cplusplus.com/reference/cctype/toupper/)
- [String Case in C++](https://www.educative.io/edpresso/string-case-in-cpp)