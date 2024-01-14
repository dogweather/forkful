---
title:    "Python: Generación de números aleatorios"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué generar números aleatorios

Generar números aleatorios es una habilidad fundamental en la programación Python, ya que permite introducir un elemento de aleatoriedad y variedad en nuestros programas. Esto puede ser útil en juegos, pruebas de rendimiento y otras aplicaciones donde se necesita tomar decisiones aleatorias. Además, es una habilidad básica para aquellos que desean adentrarse en áreas como la inteligencia artificial y el aprendizaje automático.

## Cómo hacerlo

Generar números aleatorios en Python es sencillo gracias al módulo `random`. Aquí hay un ejemplo básico para generar un número entero aleatorio entre 1 y 10:

```Python
import random

aleatorio = random.randint(1, 10)
print(aleatorio)
```

Esta es solo una de las muchas funciones que ofrece el módulo `random`. También se pueden generar números aleatorios decimales, seleccionar elementos aleatorios de una lista y más. Puedes explorar todas las opciones en la [documentación oficial de Python](https://docs.python.org/es/3/library/random.html).

## Profundizando en la generación de números aleatorios

Detrás de la aparente aleatoriedad de los números generados por la computadora, hay un algoritmo complejo que se basa en una llamada "semilla". La semilla es un número inicial que utiliza el programa para generar una secuencia de números aparentemente aleatorios. Si no se define una semilla, el módulo `random` utilizará automáticamente el reloj del sistema como semilla. Sin embargo, si se define una semilla fija, la misma secuencia de números se generará cada vez que se ejecute el programa.

También es importante destacar que la generación de números aleatorios en una computadora no es verdaderamente aleatoria, ya que es determinada por el algoritmo utilizado. Por lo tanto, no se debe utilizar para aplicaciones que requieren una aleatoriedad verdadera y segura.

## Ver también

- [Documentación oficial de Python sobre el módulo random](https://docs.python.org/es/3/library/random.html)
- [Artículo "El misterio de los números aleatorios en Python"](https://python-para-impacientes.blogspot.com/2016/08/el-misterio-de-los-numeros-aleatorios.html)
- [Video "Python Random Module"](https://www.youtube.com/watch?v=KzqSDvzOFNA&t=96s)