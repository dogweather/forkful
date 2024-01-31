---
title:                "Usando una shell interactiva (REPL)"
date:                  2024-01-26T04:12:03.021272-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando una shell interactiva (REPL)"

category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Un REPL (Read-Eval-Print-Loop, Ciclo de Leer-Evaluar-Imprimir) es un entorno de programación interactivo y simple. Los programadores lo utilizan para la experimentación en tiempo real con el lenguaje, tareas rápidas o para entender nuevos conceptos sin la carga de crear aplicaciones completas.

## Cómo hacerlo:
C++ no viene con un REPL incorporado, pero herramientas como Cling ofrecen esa capacidad. Así es cómo usar Cling para calcular la suma de dos números:

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 7;
    std::cout << "La suma es: " << a + b << std::endl;
    return 0;
}

// Salida:
// La suma es: 12
```

Inicia Cling e ingresa el código línea por línea, observando la salida después de cada comando. Es una retroalimentación inmediata, sin compilación.

## Profundización
Los REPL son comunes para lenguajes como Python o Lisp, y existen desde la década de 1960. Para C++, un lenguaje compilado, el concepto no se ajusta de manera natural, razón por la cual existen herramientas como Cling, que interpretan C++ al vuelo. Alternativas incluyen compiladores en línea o programas de prueba a pequeña escala compilados de manera tradicional. Cling está construido sobre LLVM y Clang, proporcionando un puente para que C++ se use de manera interpretada.

## Ver también
- [Cling](https://root.cern/cling/): Un intérprete interactivo de C++, construido sobre las bibliotecas LLVM y Clang.
- [Cuadernos Jupyter](https://jupyter.org/): Ofrece una carcasa interactiva dentro de un entorno de cuaderno, soporta C++ a través del núcleo xeus-cling.
- [LLVM](https://llvm.org/): Una colección de tecnologías de compiladores y cadenas de herramientas modulares y reutilizables, sobre las cuales se construye Cling.
