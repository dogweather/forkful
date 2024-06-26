---
date: 2024-01-26 04:12:03.021272-07:00
description: "C\xF3mo hacerlo: C++ no viene con un REPL incorporado, pero herramientas\
  \ como Cling ofrecen esa capacidad. As\xED es c\xF3mo usar Cling para calcular la\
  \ suma de\u2026"
lastmod: '2024-03-13T22:44:59.376891-06:00'
model: gpt-4-0125-preview
summary: C++ no viene con un REPL incorporado, pero herramientas como Cling ofrecen
  esa capacidad.
title: Usando una shell interactiva (REPL)
weight: 34
---

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
