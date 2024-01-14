---
title:                "Python: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué generar números aleatorios en Python

Generar números aleatorios es una técnica fundamental en la programación de Python. Sirve para dar variedad y dinamismo a los programas, así como para realizar pruebas y simulaciones de manera más eficiente.

## Cómo hacerlo

Para generar números aleatorios en Python, utilizamos la librería "random". Primero, debemos importarla en nuestro código de la siguiente manera:

```Python
import random
```

Una vez importada, podemos utilizar algunas de sus funciones. Por ejemplo, si queremos generar un número entero al azar entre 0 y 100, podemos utilizar la función "randint". El código sería el siguiente:

```Python
num = random.randint(0, 100)
print(num)
```

Este código imprimiría un número entero aleatorio entre 0 y 100 en cada ejecución. También podemos generar números decimales utilizando la función "uniform", de la siguiente manera:

```Python
num = random.uniform(0, 1)
print(num)
```

En este caso, se imprimirá un número decimal aleatorio entre 0 y 1 en cada ejecución del programa.

## Profundizando en la generación de números aleatorios

La librería "random" tiene una amplia variedad de funciones para generar números aleatorios en Python. Podemos especificar un rango diferente de números, así como establecer una semilla para poder repetir la misma secuencia de números aleatorios en diferentes ejecuciones del programa.

Además, es interesante tener en cuenta que los números aleatorios generados por computadora no son completamente impredecibles, ya que siguen un patrón matemático. Sin embargo, para la mayoría de los propósitos, estos números aleatorios son suficientemente aleatorios.

## Ver también

- [Documentación oficial de la librería random en Python](https://docs.python.org/es/3/library/random.html)
- [Tutorial de generación de números aleatorios en Python](https://realpython.com/python-random/)
- [Ejemplos de uso de la función random en diferentes situaciones](https://www.geeksforgeeks.org/random-module-python/)

¡Ahora que conoces cómo generar números aleatorios en Python, puedes darle más dinamismo a tus programas y realizar simulaciones de manera más eficiente!