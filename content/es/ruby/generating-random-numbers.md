---
title:    "Ruby: Generación de números aleatorios"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué

Generar números aleatorios es una técnica esencial para cualquier programador de Ruby. Puede ser utilizado en una variedad de aplicaciones, como juegos, pruebas de software y algoritmos de aprendizaje automático.

## Cómo hacerlo

La forma más sencilla de generar números aleatorios en Ruby es utilizando el método `rand`. Este método puede tomar una número opcional como límite superior, y devolverá un número aleatorio entre 0 y ese límite. Por ejemplo:

```Ruby
rand(10)
# output: 5
```

También se pueden utilizar rangos para especificar un límite inferior y superior para generar un número aleatorio. Por ejemplo:

```Ruby
rand(1..100)
# output: 67
```

Otra forma de generar números aleatorios es utilizando el método `randf`, que devuelve un número decimal entre 0 y 1. Este método puede ser útil para ciertas aplicaciones que requieren precisión decimal. Por ejemplo:

```Ruby
randf
# output: 0.754309815596807
```

## Profundizando

La generación de números aleatorios en Ruby se basa en un algoritmo de generación de números pseudoaleatorios. Esto significa que los números no son realmente aleatorios, sino que se generan a través de una fórmula matemática que utiliza un número inicial llamado "semilla". Si se utiliza la misma semilla, se obtendrá el mismo conjunto de números "aleatorios" cada vez.

Es importante tener en cuenta que el método `rand` utiliza como semilla el reloj del sistema, por lo que aunque se ejecuten dos llamadas al método en un corto periodo de tiempo, se obtendrán diferentes resultados. Sin embargo, si se quiere obtener números verdaderamente aleatorios, se puede establecer una semilla manualmente utilizando el método `srand` antes de llamar a `rand`. Por ejemplo:

```Ruby
srand 12345
rand(10)
# output: 7
rand(10)
# output: 4
```

## Ver también

- [RubyDoc: rand](https://ruby-doc.org/core-2.7.2/Random.html#method-i-rand)
- [RubyDoc: srand](https://ruby-doc.org/core-2.7.2/Random.html#method-i-srand)
- [RubyMonk: Random Numbers](https://rubymonk.com/learning/books/1-ruby-primer/chapters/10-arrays/lessons/49-random-numbers)