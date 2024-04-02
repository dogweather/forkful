---
date: 2024-01-26 01:09:15.746846-07:00
description: "Dividir el c\xF3digo en funciones significa trocear tu c\xF3digo en\
  \ fragmentos m\xE1s peque\xF1os y reutilizables. Lo hacemos para evitar la repetici\xF3\
  n, hacer nuestro\u2026"
lastmod: '2024-03-13T22:44:59.380763-06:00'
model: gpt-4-1106-preview
summary: "Dividir el c\xF3digo en funciones significa trocear tu c\xF3digo en fragmentos\
  \ m\xE1s peque\xF1os y reutilizables. Lo hacemos para evitar la repetici\xF3n, hacer\
  \ nuestro\u2026"
title: "Organizando c\xF3digo en funciones"
weight: 18
---

## ¿Qué y Por Qué?
Dividir el código en funciones significa trocear tu código en fragmentos más pequeños y reutilizables. Lo hacemos para evitar la repetición, hacer nuestro código legible y simplificar el depurado y las pruebas. Las funciones bien organizadas pueden ser como tener una caja de herramientas cuidadosamente etiquetadas, listas para usar y compartir.

## Cómo hacerlo:
Tomemos una tarea común: calcular el área de un círculo. En lugar de escribir la misma fórmula cada vez, la encapsulamos en una función.

```C++
#include <iostream>
#define PI 3.14159

double calculateCircleArea(double radio) {
    return PI * radio * radio;
}

int main() {
    double r = 5.0;
    std::cout << "Área de un círculo con radio " << r << " es " << calculateCircleArea(r) << std::endl;
    return 0;
}
```

Salida de ejemplo:
```
Área de un círculo con radio 5 es 78.5397
```

## Inmersión Profunda
Históricamente, los procedimientos y funciones fueron la columna vertebral de la programación estructurada, promovida en la década de 1960 para combatir los problemas del "código espagueti" en los lenguajes de programación imperativos anteriores. Alternativas como OOP (Programación Orientada a Objetos) van más allá al asociar estas funciones con estructuras de datos. En C++, tienes funciones regulares, métodos de clase (incluidos los métodos estáticos), lambdas y funciones de plantillas, cada una ofreciendo diferentes beneficios. Implementar funciones bien organizadas generalmente implica adherirse a principios como DRY ("Don't Repeat Yourself" o "No te repitas") y SRP (Single Responsibility Principle o Principio de Responsabilidad Única), lo que significa que cada función hace una sola cosa y la hace bien.

## Ver También
Para más sobre funciones en C++:
- https://en.cppreference.com/w/cpp/language/functions
- https://www.learncpp.com/cpp-tutorial/77-introduction-to-functions/

Para principios de diseño relacionados con funciones:
- https://es.wikipedia.org/wiki/Principio_de_responsabilidad_única
- https://es.wikipedia.org/wiki/No_te_repitas

Aprende sobre lambdas y uso avanzado de funciones:
- https://www.cprogramming.com/c++11/c++11-lambda-closures.html
- https://isocpp.org/wiki/faq/cpp14-language#lambda-captures
