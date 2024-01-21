---
title:                "Generando números aleatorios"
date:                  2024-01-20T17:49:51.583972-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generando números aleatorios"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Generar números aleatorios en Python es como tirar un dado digital. Lo hacemos porque la aleatoriedad es esencial para juegos, simulaciones y al crear datos de prueba que parezcan reales.

## Cómo:
Para generar un número aleatorio, Python cuenta con el módulo `random`. Aquí hay ejemplos sencillos:

```Python
import random

# Generar un número entero aleatorio entre 1 y 10
numero_aleatorio = random.randint(1, 10)
print(numero_aleatorio)

# Generar un número flotante entre 0 y 1
flotante_aleatorio = random.random()
print(flotante_aleatorio)
```

Output:

```
4  # Puede ser cualquier número entre 1 y 10
0.3594874628928345  # Será siempre un número entre 0 y 1
```

## Profundización:
La aleatoriedad en computación empezó con métodos físicos, como ruletas o dados, y luego se trasladó al ámbito digital. Los números generados no son verdaderamente aleatorios, sino 'pseudoaleatorios', determinados por algoritmos complejos.

En el mundo de Python, el módulo `random` es bastante usado, pero para criptografía usamos `secrets` por su mayor seguridad. Otros lenguajes tienen sus propias bibliotecas y funciones. La implementación interna de `random` se basa en un generador de números pseudoaleatorios que necesita de una semilla (`seed`) para empezar. Si usamos la misma semilla, obtendremos la misma secuencia de números.

## Ver También:
Para aprender más sobre el módulo `random` y su uso, chequea la documentación oficial: [Random Numbers in Python](https://docs.python.org/3/library/random.html).

Cuando necesites aleatoriedad criptográfica segura, vea `secrets`: [Secrets](https://docs.python.org/3/library/secrets.html).

Aquí hay un artículo útil sobre la aleatoriedad en las pruebas de software: [Randomness in Testing](https://realpython.com/python-random/).