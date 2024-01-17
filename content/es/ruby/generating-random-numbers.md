---
title:                "Generando números aleatorios"
html_title:           "Ruby: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué & porqué?
Generar números aleatorios es un concepto importante en la programación, ya que permite a los programas tomar decisiones o hacer cálculos basados en un valor aleatorio. Esto puede ser útil en juegos, simulaciones y otros escenarios en los que se requiere cierto grado de imprevisibilidad.

## Cómo:
En Ruby, podemos utilizar el método `rand` para generar números aleatorios. Este método toma un argumento opcional que especifica el rango de números que se pueden generar. Por ejemplo, el siguiente código generaría un número aleatorio entre 1 y 10:

```Ruby
rand(1..10)
```

También podemos utilizar `rand` sin un argumento para obtener un número decimal aleatorio entre 0 y 1. 

```Ruby
rand
# => 0.2543581832588834
```

## Profundizando:
El concepto de generación de números aleatorios tiene sus orígenes en la estadística y las matemáticas, y ha sido utilizado desde hace siglos en juegos de azar. En la programación, existen varias formas de generar números aleatorios, como el algoritmo de Middle Square, el algoritmo de Congruencia Lineal y el algoritmo de Mersenne Twister.

Además de `rand`, Ruby también ofrece el método `srand` para establecer una semilla (seed) inicial para la generación de números aleatorios. Esto puede ser útil para generar la misma secuencia de números aleatorios cada vez que se ejecute el programa.

## Ver también:
Si deseas saber más sobre la generación de números aleatorios en Ruby, te recomendamos los siguientes recursos:

- [Documentación oficial de Ruby sobre `rand`](https://ruby-doc.org/core/Kernel.html#method-i-rand)
- [Una explicación detallada de cómo funciona `rand`](https://www.rubyguides.com/2019/09/ruby-random/)
- [Artículo sobre algoritmos de generación de números aleatorios en Ruby](https://blog.appsignal.com/2019/01/29/generating-random-numbers-in-ruby.html)