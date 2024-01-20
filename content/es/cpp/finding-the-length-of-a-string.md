---
title:                "Encontrando la longitud de una cadena"
html_title:           "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Encontrar la longitud de una cadena en C++

## ¿Qué y por qué?

Encontrar la longitud de una cadena significa determinar el número de caracteres que contiene. Los programadores lo hacen para manipular cadenas de manera eficiente y evitar errores comunes, como el desbordamiento del búfer.

## Cómo hacerlo:

Aqui, usamos la función `length()` de la biblioteca estándar de C++ para encontrar la longitud de una cadena.

```C++
#include<iostream>
#include<string>
using namespace std;

int main() {
  string str = "Hola Mundo";
  cout<< "Longitud de cadena: " << str.length();
  return 0;
}
```
Output:
```
Longitud de cadena: 10
```

## Buceo profundo

Históricamente, en C puro, los programadores tenían que recorrer una cadena carácter por carácter para determinar su longitud utilizando el método `strlen()`. Sin embargo, C++ proporciona la función `length()` para obtener la longitud de una cadena de manera eficiente.

Alternativamente, puedes usar la función `size()` que hace exactamente lo mismo que `length()`.

Respecto a los detalles de implementación, `length()` y `size()` simplemente devuelven el valor almacenado en una variable miembro del objeto de cadena, haciendo que estos métodos sean muy eficientes ya que no necesitan recorrer toda la cadena.

## Ver también

Echa un vistazo a estos enlaces para aprender más sobre las cadenas en C++:

- [C++ Strings](http://www.cplusplus.com/reference/string/string/)
- [C++ String Class and its Applications](https://www.geeksforgeeks.org/c-string-class-and-its-applications/)