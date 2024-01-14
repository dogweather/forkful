---
title:                "Swift: Generando números aleatorios."
programming_language: "Swift"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

##Por qué

Generar números aleatorios es una habilidad esencial en el mundo de la programación. Puede ser útil en una variedad de situaciones, como simular pruebas, generar contraseñas seguras o crear juegos.

##Cómo hacerlo

Para generar un número aleatorio en Swift, podemos usar la función `arc4random_uniform()` que toma un número entero como argumento y devuelve un número aleatorio entre 0 y ese número (no incluido).

```Swift
let randomNumber = arc4random_uniform(10)
print(randomNumber) // Output: Un número aleatorio entre 0 y 10
```

También podemos usar la función `random()` que devuelve un número decimal entre 0 y 1.

```Swift
let randomDecimal = random()
print(randomDecimal) // Output: Un número decimal aleatorio entre 0 y 1
```

Si queremos generar un número aleatorio en un rango específico, podemos usar la función `random(in:)` y pasarle un rango como argumento.

```Swift
let randomInRange = random(in: 1...10)
print(randomInRange) // Output: Un número aleatorio entre 1 y 10
```

##Profundizando

Existen diferentes formas de generar números aleatorios en Swift y cada una tiene sus ventajas y desventajas en términos de distribución y complejidad. Además, hay formas de mejorar la aleatoriedad de los números generados, como cambiando la semilla o utilizando algoritmos de cifrado.

También es importante tener en cuenta que los números aleatorios en la programación no son completamente aleatorios, sino pseudoaleatorios, es decir, son generados por algoritmos determinísticos que simulan aleatoriedad. Por lo tanto, no deben utilizarse en situaciones que requieren una alta seguridad, como en la generación de claves criptográficas.

En resumen, la generación de números aleatorios es una habilidad importante para tener en nuestro repertorio de programación, pero debemos ser conscientes de cómo usarla adecuadamente en diferentes situaciones.

##Ver también

- [Documentación de Apple sobre la generación de números aleatorios en Swift](https://developer.apple.com/documentation/swift/1541126-random)
- [Artículo sobre la aleatoriedad en la programación](https://www.freecodecamp.org/news/how-computers-generate-random-numbers-695c1651b0ae/)
- [Tutorial de Swift sobre cómo generar números aleatorios en un rango específico](https://www.hackingwithswift.com/articles/205/how-to-generate-random-numbers-in-swift)