---
date: 2024-01-26 03:36:43.425116-07:00
description: "C\xF3mo hacerlo: Supongamos que tienes un bloque de c\xF3digo que calcula\
  \ e imprime el \xE1rea y el per\xEDmetro de un rect\xE1ngulo dados su longitud y\
  \ anchura. Hace el\u2026"
lastmod: '2024-03-13T22:44:58.620987-06:00'
model: gpt-4-0125-preview
summary: "Supongamos que tienes un bloque de c\xF3digo que calcula e imprime el \xE1\
  rea y el per\xEDmetro de un rect\xE1ngulo dados su longitud y anchura."
title: "Refactorizaci\xF3n"
weight: 19
---

## Cómo hacerlo:
Supongamos que tienes un bloque de código que calcula e imprime el área y el perímetro de un rectángulo dados su longitud y anchura. Hace el trabajo, pero es repetitivo y un poco desordenado.

```python
# Versión Original
length = 4
width = 3

# Calcular área y perímetro
area = length * width
perimeter = 2 * (length + width)

print("Área:", area)
print("Perímetro:", perimeter)
```

Podemos refactorizar esto encapsulando la funcionalidad en funciones, lo que hace que el código sea más organizado y reutilizable:

```python
# Versión Refactorizada

def calcular_area(length, width):
    return length * width

def calcular_perimetro(length, width):
    return 2 * (length + width)

# uso
length = 4
width = 3

print("Área:", calcular_area(length, width))
print("Perímetro:", calcular_perimetro(length, width))
```

Ambos fragmentos de código producen el mismo resultado:
```
Área: 12
Perímetro: 14
```

Pero la versión refactorizada es más limpia y separa las preocupaciones, facilitando la actualización de un cálculo sin afectar al otro.

## Inmersión Profunda
La refactorización tiene sus raíces en los primeros días de la ingeniería de software, cuando los programadores se dieron cuenta de que el código podía—y debería—ser mejorado incluso si ya "funciona". El libro seminal de Martin Fowler "Refactoring: Improving the Design of Existing Code" articuló muchos principios y técnicas fundamentales. Él famosamente dijo, "Cualquier tonto puede escribir código que una computadora puede entender. Los buenos programadores escriben código que los humanos pueden entender."

Las alternativas a la refactorización podrían incluir reescribir el código desde cero o hacer ajustes menores sin mejora sistemática. Sin embargo, la refactorización suele ser más rentable que una reescritura y menos arriesgada que las modificaciones ad-hoc. Los detalles de implementación pueden ser específicos para cada paradigma de programación; sin embargo, la programación orientada a objetos se presta particularmente bien para la refactorización, especialmente con técnicas como la extracción de métodos (como nuestras funciones `calcular_area` y `calcular_perimetro`), la inclusión en línea, el traslado de características entre objetos y el renombramiento de métodos o variables por claridad.

La refactorización en Python a menudo utiliza herramientas como `PyCharm`, que tiene capacidades de refactorización incorporadas, o `rope`, una biblioteca de Python específicamente diseñada para la refactorización. Se aconseja encarecidamente el uso cuidadoso del control de versiones, como `git`, durante la refactorización para realizar un seguimiento de los cambios de forma incremental.

## Ver También
Para aquellos que buscan más, sumérjanse en los siguientes recursos:
- El libro de Martin Fowler: [Refactoring: Improving the Design of Existing Code](http://www.refactoring.com/)
- Refactorización en Python con `rope`: [GitHub - rope](https://github.com/python-rope/rope)
- Documentación sobre refactorización de PyCharm: [Jetbrains PyCharm Refactoring Source Code](https://www.jetbrains.com/help/pycharm/refactoring-source-code.html)
- Refactoring.guru: [Refactoring and Design Patterns](https://refactoring.guru/refactoring)
- Conferencias sobre Código Limpio de Uncle Bob (Robert C. Martin): [Clean Code - Uncle Bob / Lección 1](https://www.youtube.com/watch?v=7EmboKQH8lM)
