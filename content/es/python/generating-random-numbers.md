---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
La generación de números aleatorios implica producir una secuencia de números que no tienen patrón predictable. Programadores lo hacen para cosas como pruebas automatizadas, simulaciones, y juegos.

## Cómo hacerlo:
El módulo `random` de Python provee funciones para generar números aleatorios. Aquí hay un ejemplo:

```Python
import random

# Generar un número flotante aleatorio entre 0 y 1
num_aleatorio = random.random()
print(num_aleatorio)
```

Este código imprimirá un número flotante, como `0.4356684508793945` por ejemplo.

Podemos usar `randrange` para obtener un número entero dentro de un rango específico:

```Python
# Generar un número entero aleatorio entre 1 y 10
num_aleatorio = random.randrange(1, 11)
print(num_aleatorio)
```

Esto imprimirá un número entero entre 1 y 10, como `7` por ejemplo.

## Inmersión Profunda
La generación de números aleatorios tiene una historia fascinante en computación. Aunque los números generados por computadoras no son verdaderamente aleatorios y son más correctamente llamados "pseudoaleatorios", son suficientemente aleatorios para la mayoría de las aplicaciones.

Existen alternativas al módulo `random` de Python. Por ejemplo, `numpy.random` es una opción popular en el ámbito de la ciencia y la analítica de datos debido a su compatibilidad con los arrays de numpy.

Los detalles de implementación detrás de la generación de números aleatorios pueden ser bastante complejos, involucrando conceptos matemáticos avanzados y algoritmos. En esencia, Python usa un algoritmo conocido como Mersenne Twister para generar números pseudoaleatorios.

## Más Información
Para más detalles sobre la generación de números aleatorios en Python, visita la documentación oficial de Python:

- Módulo `random`: https://docs.python.org/3/library/random.html
- Módulo `numpy.random`: https://numpy.org/doc/1.16/reference/routines.random.html