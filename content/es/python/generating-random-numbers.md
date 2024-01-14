---
title:                "Python: Generando números aleatorios"
programming_language: "Python"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

# ¿Por qué generar números aleatorios en Python?

Generar números aleatorios en Python es una técnica comúnmente utilizada en programación para crear resultados impredecibles y aleatorios. Puede ser útil en muchas aplicaciones, como juegos, simulaciones y pruebas de software.

## ¿Cómo hacerlo?

Generar números aleatorios en Python es bastante sencillo. La librería `random` proporciona una serie de funciones para hacerlo. A continuación se muestra un ejemplo básico utilizando la función `random.randint()` para generar un número aleatorio entre 1 y 10.

```python
import random

numero_aleatorio = random.randint(1, 10)
print(numero_aleatorio)
```
**Output:**

> 7

También se pueden generar listas de números aleatorios utilizando la función `random.sample()`, como se muestra en el siguiente ejemplo:

```python
import random

lista_aleatoria = random.sample(range(100), 10)
print(lista_aleatoria)
```
**Output:**

> [42, 72, 16, 85, 30, 54, 61, 35, 93, 49]

Estas son solo algunas de las funciones disponibles en la librería `random`. Asegúrate de consultar la documentación oficial para más información y opciones.

## Profundizando en la generación de números aleatorios

Si deseas tener un mayor control sobre los números aleatorios que generas, puedes utilizar la función `random.seed()` para establecer una semilla. Esto significa que cada vez que ejecutes el programa, obtendrás los mismos números aleatorios en el mismo orden. Esto puede ser útil para pruebas y depuración.

También es importante tener en cuenta que, aunque los números generados con `random` pueden parecer realmente aleatorios, en realidad son pseudoaleatorios. Esto significa que están basados en un algoritmo matemático y una semilla inicial. Por lo tanto, si utilizas la misma semilla, obtendrás los mismos números.

# Ver también

- Documentación oficial de la librería `random` en Python: https://docs.python.org/es/3/library/random.html
- Tutorial sobre generación de números aleatorios en Python: https://www.freeCodeCamp.org/news/how-to-generate-random-numbers-in-python/
- Ejemplos prácticos de generación de números aleatorios en Python: https://realpython.com/python-random/