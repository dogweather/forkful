---
title:    "Elm: Generando números aleatorios"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por qué

Generar números aleatorios es una habilidad importante para cualquier programador, ya que permite agregar un elemento de aleatoriedad a nuestras aplicaciones, lo que las hace más interesantes y atractivas para los usuarios. Además, puede ser útil en diferentes casos, como en juegos o simulaciones.

## Cómo hacerlo

Para generar números aleatorios en Elm, podemos utilizar la función `Random.generate` junto con la función `Random.int`, que nos permite generar un número entero aleatorio dentro de un rango dado. Por ejemplo:

```Elm
Random.generate Random.int (1, 10)

-- Output: 7 (puede variar en cada ejecución)
```

También podemos obtener una lista de números aleatorios utilizando la función `Random.list` y especificando el número de elementos deseado, así como el generador de números aleatorios que queremos utilizar. Por ejemplo:

```Elm
Random.generate (Random.list 5 Random.float)  -- Genera una lista de 5 números aleatorios tipo float
-- Output: [0.562, 0.320, -0.820, 0.124, -1.055] (puede variar en cada ejecución)
```

Otra opción es utilizar la función `Random.generateSeed` para generar una semilla aleatoria que luego podemos usar para producir una secuencia de números aleatorios. Esto puede ser útil en aplicaciones que requieren la misma secuencia de números aleatorios cada vez que se ejecutan. Por ejemplo:

```Elm
seedGenerator = Random.generateSeed
-- Esta función no tiene ningún efecto sobre nuestra aplicación, solo devuelve una semilla aleatoria

Random.generate (Random.initialSeed 5) (Random.float)  -- Genera un número tipo float utilizando la semilla generada anteriormente
-- Output: (0.514, 5) (puede variar en cada ejecución, pero siempre será el mismo con la semilla 5)
```

## Profundizando

Los generadores de números aleatorios en Elm tienen algunas características interesantes, como la función `Random.pair`, que nos permite generar una tupla de dos valores aleatorios, o la función `Random.map` que nos permite aplicar una función a los resultados generados.

También es importante tener en cuenta que los números aleatorios generados en Elm son pseudoaleatorios, lo que significa que están determinados por la semilla inicial que les proporcionamos. Esto puede ser útil para poder reproducir una secuencia de números aleatorios, pero también lo hace menos seguro para usos criptográficos.

## Vea también

- Documentación de Elm: https://guide.elm-lang.org/
- Ejemplos de generación de números aleatorios en Elm: https://elmprogramming.com/random-numbers.html
- Tutorial interactivo sobre generación de números aleatorios en Elm: https://elmprogramming.com/interactive-tutorial-introducing-random-in-elm.html