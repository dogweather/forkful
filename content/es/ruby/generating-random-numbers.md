---
title:                "Generando números aleatorios"
date:                  2024-01-20T17:50:34.181885-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generando números aleatorios"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Generar números aleatorios es crear valores numéricos por azar, sin predecir su secuencia. Los programadores usan esto para cosas como pruebas de software, juegos, simulaciones o seguridad en criptografía.

## Cómo hacerlo:
Para generar un número aleatorio en Ruby, puedes utilizar el método `rand`. Aquí hay algunos ejemplos:

```Ruby
# Un número aleatorio entre 0 y 1 (excluyendo el 1):
aleatorio = rand
puts aleatorio

# Un número entero aleatorio entre 0 y 10 (incluyendo el 10):
entero_aleatorio = rand(11)
puts entero_aleatorio

# Un número aleatorio dentro de un rango:
en_rango = rand(1..20)
puts en_rango
```

Ejemplo de salida:
```
0.43762849217395735
7
15
```

## Deep Dive:
La generación de números aleatorios tiene una larga historia, influenciada por las matemáticas y la estadística. En Ruby, la clase `Random` incluye métodos para generar estos números. Internamente, Ruby puede usar diferentes algoritmos para generar la aleatoriedad, como Mersenne Twister, conocido por su rapidez y uniformidad de distribución.

Hay alternativas al método `rand`, como `SecureRandom` para cuando necesitas una seguridad más robusta, ya que provee una aleatoriedad menos predecible.

La implementación de la generación de números aleatorios en Ruby også resguarda el estado del "generador", permitiendo que los programas puedan controlar la reproducibilidad de la secuencia de números aleatorios, algo crucial para las pruebas.

## See Also:
- La documentación oficial de Ruby sobre la clase [Random](https://ruby-doc.org/core-3.1.0/Random.html).
- [SecureRandom](https://ruby-doc.org/stdlib-3.1.0/libdoc/securerandom/rdoc/SecureRandom.html) para necesidades de seguridad mejoradas.
- Un vistazo a [Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister) para entender el algoritmo detrás de muchas implementaciones de generación de números aleatorios.