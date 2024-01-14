---
title:                "Ruby: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué

Generar números aleatorios es una habilidad muy útil para un programador de Ruby. Al generar números aleatorios, puedes simular situaciones del mundo real, crear juegos o incluso mejorar la seguridad de tus aplicaciones.

## Cómo hacerlo

Para generar un número aleatorio en Ruby, puedes utilizar el método `rand`, que toma como argumento un rango y devuelve un número aleatorio dentro de ese rango. Por ejemplo:

```ruby
rand(1..10)
# Output: 6
```

También puedes utilizar el método `rand` sin argumentos, lo que devolverá un número aleatorio entre 0 y 1. Por ejemplo:

```ruby
rand
# Output: 0.342559771084647
```

Puedes generar un número aleatorio de un array utilizando el método `sample`, que elegirá un elemento aleatorio del array. Por ejemplo, si tenemos un array de colores:

```ruby
colores = ["rojo", "azul", "verde"]

colores.sample
# Output: "verde"
```

## Profundizando

Si quieres generar números aleatorios con una semilla específica, puedes utilizar el método `srand` antes de llamar a `rand`. La semilla es un número que inicializa el generador de números aleatorios, y cuando se usa la misma semilla, se generará la misma secuencia de números aleatorios. Por ejemplo:

```ruby
srand(1234)
rand(1..10)
# Output: 8

srand(1234)
rand(1..10)
# Output: 8 (mismo número que antes)
```

También puedes generar una secuencia de números aleatorios utilizando un bloque y el método `Random.new`. Por ejemplo:

```ruby
Random.new.seed # inicializa el generador de números aleatorios con una semilla aleatoria

Random.new.rand(1..10)
# Output: 5 (un número aleatorio dentro del rango)

Random.new.rand(1..10)
# Output: 9 (otro número aleatorio dentro del rango)
```

## Ver también

- [Documentación de Ruby sobre el método `rand`](https://ruby-doc.org/core-2.7.2/Random.html#method-c-rand)
- [Tutorial de DigitalOcean sobre cómo generar números aleatorios en Ruby](https://www.digitalocean.com/community/tutorials/how-to-generate-random-numbers-in-ruby-es)