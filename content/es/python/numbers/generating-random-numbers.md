---
date: 2024-01-27 20:35:03.047403-07:00
description: "C\xF3mo hacerlo: Python proporciona el m\xF3dulo `random` que ayuda\
  \ en la generaci\xF3n de n\xFAmeros aleatorios para varios usos. Aqu\xED est\xE1\
  \ c\xF3mo comenzar: 1.\u2026"
lastmod: '2024-03-13T22:44:58.606563-06:00'
model: gpt-4-0125-preview
summary: "Python proporciona el m\xF3dulo `random` que ayuda en la generaci\xF3n de\
  \ n\xFAmeros aleatorios para varios usos."
title: "Generaci\xF3n de n\xFAmeros aleatorios"
weight: 12
---

## Cómo hacerlo:
Python proporciona el módulo `random` que ayuda en la generación de números aleatorios para varios usos. Aquí está cómo comenzar:

1. **Importar el módulo**
    ```Python
    import random
    ```

2. **Generar un Entero Aleatorio**
    Entre cualquier par de números.
    ```Python
    random_integer = random.randint(1, 10)
    print(random_integer)
    ```
    Salida de ejemplo: `7`

3. **Generar un Flotante**
    Entre 0 y 1.
    ```Python
    random_float = random.random()
    print(random_float)
    ```
    Salida de ejemplo: `0.436432634653`

    Si necesitas un flotante en un rango diferente, multiplica:
    ```Python
    random_float_range = random.random() * 5  # 0 a 5
    print(random_float_range)
    ```
    Salida de ejemplo: `3.182093745`

4. **Elegir un Elemento Aleatorio de una Lista**
    ```Python
    greetings = ['Hello', 'Hi', 'Hey', 'Hola', 'Bonjour']
    print(random.choice(greetings))
    ```
    Salida de ejemplo: `Hola`

5. **Barajar una Lista**
    Perfecto para juegos de cartas o cualquier aplicación que necesite aleatorizar el orden.
    ```Python
    numbers = list(range(10))
    random.shuffle(numbers)
    print(numbers)
    ```
    Salida de ejemplo: `[2, 5, 0, 4, 9, 8, 1, 7, 6, 3]`

## Profundización
El módulo `random` en Python usa un generador de números pseudoaleatorios (PRNG), específicamente el algoritmo Mersenne Twister, que es bueno para aplicaciones de propósito general pero no es adecuado para fines criptográficos debido a su previsibilidad si se observan suficientes salidas. El módulo `secrets`, introducido en Python 3.6, ofrece una alternativa mejor para generar números aleatorios fuertemente criptográficos, especialmente útil en aplicaciones sensibles a la seguridad. Por ejemplo, generar un token aleatorio seguro para un enlace de restablecimiento de contraseña:

```Python
import secrets
token = secrets.token_hex(16)
print(token)
```

Históricamente, generar números aleatorios que sean verdaderamente aleatorios ha sido un desafío en la informática, con métodos tempranos que dependían de fenómenos físicos o semillas ingresadas manualmente. El desarrollo y adopción de algoritmos como Mersenne Twister (usado por defecto en el módulo `random` de Python hasta al menos mi última actualización de conocimiento en 2023) marcó un progreso significativo. Sin embargo, la búsqueda continua de algoritmos más seguros y eficientes ha llevado a la inclusión del módulo `secrets` para tareas relacionadas con la criptografía. Esta evolución refleja la creciente importancia de la seguridad en el desarrollo de software y la necesidad de una aleatoriedad más robusta en aplicaciones que van desde la encriptación hasta la generación de tokens seguros.
