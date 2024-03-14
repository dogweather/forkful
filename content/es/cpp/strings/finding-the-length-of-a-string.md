---
date: 2024-01-20 17:47:13.485545-07:00
description: "Encontrar la longitud de una cadena es calcular cu\xE1ntos caracteres\
  \ contiene. Los programadores lo hacen para, por ejemplo, validar entradas, bucles\
  \ de\u2026"
lastmod: '2024-03-13T22:44:59.366583-06:00'
model: gpt-4-1106-preview
summary: "Encontrar la longitud de una cadena es calcular cu\xE1ntos caracteres contiene.\
  \ Los programadores lo hacen para, por ejemplo, validar entradas, bucles de\u2026"
title: Calculando la longitud de una cadena
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Encontrar la longitud de una cadena es calcular cuántos caracteres contiene. Los programadores lo hacen para, por ejemplo, validar entradas, bucles de procesamiento, y manipulación de texto.

## Cómo hacerlo:

C++ moderno nos ofrece opciones sencillas:

```C++
#include <iostream>
#include <string>

int main() {
    std::string texto = "Hola Mundo";
    std::cout << "La longitud de la cadena es: " << texto.length() << std::endl;
    // También puedes usar texto.size()
    std::cout << "La longitud usando size() es: " << texto.size() << std::endl;
    return 0;
}

/* Salida:
La longitud de la cadena es: 10
La longitud usando size() es: 10
*/
```

## Profundizando

Históricamente, las cadenas en C se manejaban como arrays de `char` terminados en `NULL`, y la función `strlen()` de la biblioteca estándar de C se necesitaba para encontrar su longitud. En C++, `std::string` maneja el tamaño internamente, haciendo que `length()` y `size()` sean opciones directas y seguras; ambas devuelven un `size_t`, una representación sin signo del tamaño.

Existen alternativas como usar la función `std::distance(begin(cadena), end(cadena))` para containers genéricos, pero para `std::string` las funciones miembro son más convenientes.

La implementación interna de `length` o `size` en `std::string` suele ser de complejidad constante O(1), ya que no recorrer la cadena, sino que retorna una variable interna que lleva la cuenta de la longitud.

## Ver También

- Documentación oficial de `std::string::size`: https://en.cppreference.com/w/cpp/string/basic_string/size
- Documentación oficial de `std::string::length`: https://en.cppreference.com/w/cpp/string/basic_string/length
- Una discusión en Stack Overflow sobre `length()` vs `size()`: https://stackoverflow.com/questions/905479/stdstringlength-and-stdstringsize-differences
- La historia de las cadenas en C: https://en.wikipedia.org/wiki/C_string_handling
