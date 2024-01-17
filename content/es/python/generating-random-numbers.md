---
title:                "Generando números aleatorios"
html_title:           "Python: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Generar números aleatorios es el proceso de generar valores numéricos que parecen ser aleatorios. Los programadores hacen esto para agregar variedad e imprevisibilidad a sus aplicaciones y juegos.

## Cómo hacerlo:

Generar un número aleatorio en Python es muy sencillo. Primero, importamos el módulo `random`:

```Python
import random
```

Luego, podemos utilizar la función `random()` para generar un número decimal aleatorio entre 0 y 1:

```Python
num = random.random()
```

También podemos generar un número aleatorio entero utilizando la función `randint()` y especificando el rango de valores en el que queremos que se genere el número (incluyendo el número final):

```Python
num = random.randint(1, 10)
```

## Profundizando:

El concepto de generación de números aleatorios ha existido desde la antigüedad, pero con el avance de la tecnología, se han desarrollado algoritmos más complejos y eficientes para generar números realmente aleatorios. Algunas alternativas populares a `random` en Python incluyen `numpy.random`, que ofrece más funciones para generar números aleatorios de diferentes distribuciones.

En cuanto a la implementación, la función `random()` en Python realmente no genera un número verdaderamente aleatorio. En su lugar, utiliza un algoritmo matemático para producir una secuencia de números que parecen aleatorios pero son en realidad predecibles. Por lo tanto, si necesitas una verdadera aleatoriedad, es mejor utilizar un generador externo de números aleatorios y no confiar en el módulo `random` de Python.

## Ver también:

- Documentación oficial de Python para `random`: https://docs.python.org/es/3/library/random.html
- Ejemplos prácticos de uso de `random`: https://realpython.com/python-random/
- Alternativas a `random` en Python: https://stackoverflow.com/questions/1471564/whats-the-best-way-to-generate-random-strings-of-a-specific-length-in-python