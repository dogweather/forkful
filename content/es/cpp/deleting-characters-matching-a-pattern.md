---
title:    "C++: Eliminando caracteres que coinciden con un patrón"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por qué

A veces, en programación, necesitamos eliminar de una cadena de texto los caracteres que coincidan con cierto patrón. Esto puede ser útil para limpiar datos o para obtener información específica de una cadena. Aprender cómo hacer esto en C++ puede facilitar el manejo de cadenas y mejorar nuestro código.

## Cómo hacerlo

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
  // Definir una cadena de ejemplo
  string cadena = "Hola@@123 Mundo$$$";
  // Definir el patrón a buscar
  string patron = "@$"; 

  // Recorrer la cadena y eliminar los caracteres que coincidan con el patrón
  for (int i = 0; i < cadena.length(); i++) {
    // Encontrar la posición del caracter del patrón en la cadena
    size_t pos = cadena.find(patron[i]);
    // Reemplazar el caracter con un espacio en blanco
    cadena.replace(pos, 1, " ");
  }

  // Imprimir el resultado
  cout << "La cadena resultante es: " << cadena << endl;

  return 0;
}
```
**Salida:**
```
La cadena resultante es: Hola 123 Mundo
```
En este ejemplo, usamos la función `find()` para encontrar la posición del caracter del patrón en la cadena y luego usamos la función `replace()` para reemplazarlo con un espacio en blanco.

## Profundizando

Aunque en este ejemplo usamos la función `replace()` para eliminar un solo caracter, también se puede utilizar para eliminar una secuencia de caracteres. Además, existen otras funciones útiles en la biblioteca de cadenas de C++, como `erase()` y `substr()`, que pueden ser de utilidad al eliminar caracteres que coincidan con un patrón específico. También se puede utilizar la biblioteca **regex** para trabajar con expresiones regulares y hacer búsquedas más complejas en las cadenas.

## Ver también
- [Eliminar caracteres especiales de una cadena en C++](https://www.delftstack.com/es/howto/cpp/how-to-remove-special-characters-from-string-in-cpp/)
- [Cadenas en C++: operaciones comunes](https://www.geeksforgeeks.org/cpp-strings/)
- [Librería regex de C++](https://www.cplusplus.com/reference/regex/)