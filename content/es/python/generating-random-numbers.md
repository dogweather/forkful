---
title:    "Python: Generando números aleatorios"
keywords: ["Python"]
---

{{< edit_this_page >}}

#¿Por qué generar números aleatorios?

Generar números aleatorios es una habilidad esencial para cualquier programador de Python. Estos números se utilizan en una amplia gama de escenarios, desde juegos hasta algoritmos complejos. Aprender cómo generar números aleatorios te permitirá crear programas más dinámicos y versátiles.

## Cómo hacerlo

La generación de números aleatorios en Python se puede lograr fácilmente utilizando la biblioteca estándar `random`. A continuación se muestra un ejemplo básico de cómo generar un número aleatorio entre 1 y 10 y cómo imprimirlo en pantalla:

```Python
import random

num = random.randint(1, 10)
print(num)
```

La salida de este código será un número aleatorio entre 1 y 10 cada vez que se ejecute.

También se pueden generar listas aleatorias usando la función `sample()` de la biblioteca `random`. Por ejemplo, para crear una lista de 5 números aleatorios entre 1 y 50, se puede usar el siguiente código:

```Python
import random

lista = random.sample(range(1, 50), 5)
print(lista)
```

La salida será una lista de 5 números aleatorios sin duplicados.

## Profundizando

El proceso de generación de números aleatorios se basa en algoritmos matemáticos. Estos algoritmos utilizan una semilla (o número inicial) para producir una secuencia de números pseudoaleatorios. Esto significa que, si se utiliza la misma semilla, se obtendrá la misma secuencia de números. La biblioteca `random` de Python tiene una función `seed()` que permite especificar la semilla a utilizar. Sin embargo, la mayoría de las veces se utiliza una semilla predeterminada para generar números verdaderamente aleatorios.

También es importante tener en cuenta que los números generados aleatoriamente no son realmente aleatorios, sino que siguen una distribución probabilística. Es decir, algunos números tienen más posibilidades de ser generados que otros. Por ejemplo, si se genera un número aleatorio entre 1 y 10, es más probable que salga un 5 que un 1 o un 10.

# Ver también

- Documentación oficial de la biblioteca `random` de Python: https://docs.python.org/es/3/library/random.html
- Tutorial de Real Python sobre generación de números aleatorios en Python: https://realpython.com/python-random/
- Artículo sobre la verdadera aleatoriedad en la generación de números aleatorios: https://www.bitdegree.org/learn/random-number-generation-python