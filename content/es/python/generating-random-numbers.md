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

## ¿Por qué?

Generar números aleatorios es una técnica muy común en programación y puede ser útil en varias situaciones. Puede ser utilizado para pruebas de código, creación de juegos o simulaciones, entre otras cosas. También puede ser una forma divertida de explorar y aprender sobre los conceptos de probabilidad y aleatoriedad en la programación.

## Cómo hacerlo

Para generar números aleatorios en Python, podemos utilizar la librería `random`. Primero, necesitamos importarla en nuestro código:

```Python
import random
```

Una vez que tenemos la librería, podemos utilizar la función `random()` para generar un número aleatorio decimal entre 0 y 1:

```Python
numero_aleatorio = random.random()
print(numero_aleatorio)
```

También podemos especificar un rango utilizando las funciones `randint()` o `uniform()`:

```Python
# Generar un número aleatorio entre 1 y 10
numero_entre_1_y_10 = random.randint(1, 10)
print(numero_entre_1_y_10)

# Generar un número aleatorio decimal entre 1 y 10
numero_con_decimales = random.uniform(1, 10)
print(numero_con_decimales)
```

Además, podemos utilizar la función `choice()` para seleccionar un elemento aleatorio de una lista o cadena de texto:

```Python
# Seleccionar un nombre aleatorio de una lista
nombres = ['María', 'Juan', 'Ana', 'Pablo']
nombre_aleatorio = random.choice(nombres)
print(nombre_aleatorio)

# Seleccionar una letra aleatoria de una cadena
letra_aleatoria = random.choice('Hola')
print(letra_aleatoria)
```

## Profundizando

La librería `random` utiliza un algoritmo llamado "Generador de Números Pseudoaleatorios" (PRNG por sus siglas en inglés) para crear secuencias de números aparentemente aleatorios. Estos números no son realmente aleatorios, ya que se generan por computadora, pero siguen un patrón matemático predecible. Esto significa que si volvemos a ejecutar el código, obtendremos los mismos números en el mismo orden.

Para evitar esto, podemos utilizar la función `seed()` para establecer una "semilla" que cambiará la secuencia de números generados en cada ejecución:

```Python
# Establecer una semilla con el valor "123"
random.seed(123)

# Generar 5 números aleatorios
for i in range(5):
    print(random.random())
```

Además, podemos utilizar la función `shuffle()` para mezclar los elementos de una lista de manera aleatoria:

```Python
# Lista desordenada
numeros = [1, 2, 3, 4, 5]
print(numeros)

# Mezclar los elementos de la lista
random.shuffle(numeros)
print(numeros)
```

Ahora que sabemos cómo generar números aleatorios en Python, podemos usar esta técnica en nuestros proyectos para hacerlos más interesantes y dinámicos. ¡Experimenta y diviértete con ello!

## Ver también

- Documentación oficial de la librería `random` de Python: https://docs.python.org/es/3/library/random.html
- ¿Cómo generar números aleatorios en otros lenguajes de programación?: https://www.geeksforgeeks.org/random-numbers-in-cc-python-java/