---
title:    "Kotlin: Generando números aleatorios"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por qué generar números aleatorios en Kotlin

Generar números aleatorios es una herramienta útil para cualquier programador, ya que permite crear programas más dinámicos y variados. En Kotlin, la generación de números aleatorios es una tarea sencilla gracias a sus funciones incorporadas. En este artículo, aprenderemos por qué es importante generar números aleatorios en nuestros programas y cómo hacerlo en Kotlin.

## Cómo generar números aleatorios en Kotlin

Para generar números aleatorios en Kotlin, utilizaremos la función `random()` de la clase `Random`. Esta función devuelve un número pseudoaleatorio en el rango especificado. Por ejemplo, si queremos generar un número aleatorio entre 1 y 10, podemos hacerlo de la siguiente manera:

```
val random = Random()
val randomNumber = random.nextInt(10) + 1 //Devuelve un número entre 1 y 10
println(randomNumber) //Output: 7
```

También podemos utilizar la función `nextInt(n)` para generar un número en un rango especificado. Por ejemplo, si queremos generar un número entre 100 y 200, podemos hacerlo de la siguiente manera:

```
val random = Random()
val randomNumber = random.nextInt(101) + 100 //Devuelve un número entre 100 y 200
println(randomNumber) //Output: 137
```

Otra forma de generar números aleatorios en Kotlin es utilizando la función `nextDouble()`. Esta función devuelve un número de tipo `Double` entre 0 y 1. Podemos multiplicar este número por algún valor para obtener un número aleatorio en un rango diferente.

```
val random = Random()
val randomNumber = random.nextDouble() * 50 //Devuelve un número entre 0 y 50
println(randomNumber) //Output: 23.6458
```

## Profundizando en la generación de números aleatorios en Kotlin

Kotlin utiliza el algoritmo de generación de números aleatorios de la clase `Random` llamado *Linear Congruential Generator*. Este algoritmo utiliza una fórmula matemática para producir números pseudoaleatorios. Sin embargo, es importante tener en cuenta que estos números no son verdaderamente aleatorios, sino que se basan en un patrón predecible.

Otra cosa a tener en cuenta es que la generación de números aleatorios en Kotlin depende del estado del `Random` al momento de llamar a la función `random()`. Si llamamos a la función `random()` varias veces, obtendremos los mismos números en el mismo orden. Esto se debe a que cada vez que llamamos a la función `random()`, el estado del `Random` cambia y, por lo tanto, genera un número diferente.

Para obtener una secuencia verdaderamente aleatoria, podemos utilizar un objeto `Random` diferente cada vez que llamamos a la función `random()`. También es posible establecer una semilla para el objeto `Random`, lo que asegura que siempre obtendremos la misma secuencia de números aleatorios. Esto puede ser útil para pruebas y depuración.

## Ver también

- [Documentación oficial de Random en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)

- [Generador de números aleatorios en Kotlin usando la clase Random](https://www.baeldung.com/kotlin/random)

- [Funciones de gran utilidad para generar números aleatorios en Kotlin](https://medium.com/jetbrains/quick-and-simple-functions-for-random-data-generation-in-kotlin-def9b67f4b17)