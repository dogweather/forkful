---
title:                "Escribir en el error estándar"
html_title:           "C++: Escribir en el error estándar"
simple_title:         "Escribir en el error estándar"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Escribir a la salida estándar es una técnica utilizada por los programadores para imprimir mensajes de error en la pantalla de la línea de comandos. Esto es especialmente útil cuando se están realizando pruebas y se necesitan identificar y solucionar rápidamente problemas en el código.

## ¿Cómo hacerlo?:
Para escribir a la salida estándar en C++, se utiliza la función "cerr". Aquí hay un ejemplo de cómo usarla:
```C++
#include <iostream>

using namespace std;

int main() {
  int num = 10;
  if (num % 2 == 0) {
    cerr << "El número es par" << endl;
  }
  return 0;
}
```
Salida:
```
El número es par
```

## Inmersión profunda:
Escribir a la salida estándar se remonta a los primeros días de programación, cuando los programadores no tenían herramientas avanzadas para depurar y necesitaban imprimir mensajes de error directamente en la pantalla. Hoy en día, hay otras técnicas más sofisticadas disponibles, como el uso de un depurador, pero escribir a la salida estándar sigue siendo una herramienta valiosa y fácil de implementar.