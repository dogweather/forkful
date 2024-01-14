---
title:                "Ruby: Generando números aleatorios"
programming_language: "Ruby"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

##Por qué

Generar números aleatorios es una habilidad esencial en la programación de Ruby. Con esta técnica, podemos crear juegos, aplicaciones de lotería y simulaciones de eventos aleatorios.

##Cómo hacerlo

Generar números aleatorios en Ruby es bastante sencillo. Todo lo que necesitas es la función `rand()` y, opcionalmente, puedes establecer un rango para tus números. Veamos algunos ejemplos:

```
# Generar un número aleatorio entre 1 y 10
rand(1..10)

# Generar un número entero aleatorio entre 1 y 100
rand(1..100).to_i

# Generar un número decimal aleatorio entre 0 y 1
rand()

# Generar un número decimal aleatorio entre 1 y 5
(1..5).step(0.5).to_a.sample
```

En el primer ejemplo, usamos la función `rand()` con un rango de 1 a 10. En el segundo, utilizamos `to_i` para convertir el número decimal en un entero. En el tercer ejemplo, usamos solamente `rand()` sin especificar un rango, lo que generará un número aleatorio entre 0 y 1. Y en el último ejemplo, usamos el método `step()` para definir un rango de 1 a 5 y `to_a.sample` para seleccionar un número aleatorio de ese rango.

##Profundizando

Los números aleatorios generados por la función `rand()` en Ruby no son verdaderamente aleatorios, sino que se basan en un algoritmo matemático. Esto significa que, si ejecutamos la misma función con los mismos parámetros, obtendremos el mismo resultado. Sin embargo, este algoritmo es lo suficientemente complejo como para que los resultados sean impredecibles y se aproximen a la aleatoriedad.

Si queremos una mayor precisión en nuestros números aleatorios, podemos utilizar la gema `securerandom` en lugar de la función `rand()`. Esta gema utiliza información del sistema operativo para generar números aleatorios más seguros. También podemos utilizar otros métodos para obtener números aleatorios, como utilizando la hora actual o la dirección IP.

En resumen, la generación de números aleatorios en Ruby es una habilidad esencial para cualquier programador, ya sea para realizar simulaciones o crear juegos y aplicaciones de lotería. Con la función `rand()` y otras técnicas, podemos crear resultados impredecibles y acercarnos a la verdadera aleatoriedad.

##Mira también
- [Documentación oficial de Ruby sobre la función `rand()`](https://ruby-doc.org/core-2.7.0/Random.html#method-i-rand)
- [Gema SecureRandom](https://rubygems.org/gems/securerandom)
- [Artículo de Medium sobre otros métodos de generación de números aleatorios](https://medium.com/tech-traversal/random-number-generation-methods-in-ruby-5c30cc78a0d)