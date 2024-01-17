---
title:                "Creando pruebas"
html_title:           "C++: Creando pruebas"
simple_title:         "Creando pruebas"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir pruebas en programación se trata de crear pequeños programas que comprueban si tu código funciona como debería. Los programadores lo hacen para asegurarse de que su código es robusto y libre de errores, lo que a su vez garantiza una mejor experiencia para los usuarios.

## Cómo:

```C++
#include <iostream>

int main() {
  int num1 = 5;
  int num2 = 10;
  
  // Prueba de que la suma de dos números es correcta
  if (num1 + num2 == 15) {
    // Si la prueba es exitosa, la consola imprimirá "La suma es correcta"
    std::cout << "La suma es correcta" << std::endl;
  } else {
    // Si la prueba falla, la consola imprimirá "La suma es incorrecta"
    std::cout << "La suma es incorrecta" << std::endl;
  }
  
  return 0;
}
```

**Output: "La suma es correcta"**

## Profundizando:

Escribir pruebas es una práctica común en el desarrollo de software moderno. Aunque puede parecer un proceso tedioso, las pruebas automatizadas son una forma eficiente de detectar errores y garantizar que el código sea estable y funcional. Alternativas a la escritura de pruebas incluyen pruebas manuales, pero estas pueden ser mucho más lentas y propensas a errores humanos. Además, existen herramientas y marcos de prueba que pueden facilitar y agilizar el proceso de escritura de pruebas en C++. 

## Ver también:

- [Documentación oficial de Google sobre cómo escribir pruebas en C++](https://google.github.io/googletest/)
- [Introducción a las pruebas unitarias en C++](https://guides.github.com/features/mastering-markdown/)