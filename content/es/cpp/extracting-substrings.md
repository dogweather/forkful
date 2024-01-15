---
title:                "Extracción de subcadenas"
html_title:           "C++: Extracción de subcadenas"
simple_title:         "Extracción de subcadenas"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Extraer subcadenas es una tarea muy común en la programación, especialmente cuando trabajamos con cadenas de texto. Nos permite obtener parte de una cadena más grande y utilizarla de manera independiente, lo que puede ser muy útil en diversos escenarios.

## Cómo

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  // Definimos una cadena de texto
  string frase = "Este es un ejemplo de frase";

  // Extraer una subcadena desde la posición 8 hasta el final
  string subcadena1 = frase.substr(8);
  cout << subcadena1 << endl;
  // Output: un ejemplo de frase

  // Extraer una subcadena de 10 caracteres desde la posición 5
  string subcadena2 = frase.substr(5, 10);
  cout << subcadena2 << endl;
  // Output: es un ejemplo

  // Extraer una subcadena delimitada por un caracter específico
  string subcadena3 = frase.substr(5, frase.find("de") - 1);
  cout << subcadena3 << endl;
  // Output: es un ejemplo

  return 0;
}
```

## Profundizando

La función `substr()` en C++ acepta dos argumentos: la posición inicial y la longitud de la subcadena que deseamos extraer. También podemos utilizar la función `find()` para encontrar la posición de un carácter específico en una cadena y utilizarla como argumento en `substr()`.

Además, es importante destacar que las cadenas de texto en C++ son inmutables, es decir, no se pueden modificar una vez creadas. Por lo tanto, cuando extraemos una subcadena, se crea una nueva cadena en lugar de modificar la original.

## Ver también

- [Función substr() en C++](https://www.cplusplus.com/reference/string/string/substr/)
- [Split y join en cadenas de texto en C++](https://www.geeksforgeeks.org/split-a-string-in-cpp/)
- [Documentación de cadenas de texto en C++](https://www.cplusplus.com/reference/string/)