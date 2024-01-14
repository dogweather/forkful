---
title:    "Swift: Generando números aleatorios"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por qué generar números aleatorios en Swift?

Generar números aleatorios es una habilidad esencial para cualquier programador, ya que permite crear aplicaciones más dinámicas e interactivas. Además, en Swift, hay varias maneras de generar números aleatorios, lo que lo convierte en una tarea fácil y divertida de aprender.

## Cómo hacerlo en Swift

Para generar números aleatorios en Swift, hay dos formas principales de hacerlo: utilizando el método `randomElement()` de la clase `Array` o el método `random()` de la clase `Int` o `Double`.

- Utilizando `randomElement()`:
```Swift
let array = [1, 2, 3, 4, 5] 
let randomElement = array.randomElement() // Obtener un elemento aleatorio del array
print(randomElement) // Salida: un número aleatorio del array, por ejemplo: 3
```

- Utilizando `random()`:
```Swift
let randomNumber = Int.random(in: 1...10) // Generar un número entero aleatorio entre 1 y 10
print(randomNumber) // Salida: un número aleatorio entre 1 y 10, por ejemplo: 7
```

## Profundizando en la generación de números aleatorios en Swift

Para una mayor personalización, en Swift también podemos especificar el rango de números aleatorios que queremos generar, utilizando el método `random(in:)`. Además, también es posible utilizar `shuffle()` para mezclar los elementos de un array de manera aleatoria.

Otra opción interesante es utilizar `randomSource` de la clase `Generator` para crear una fuente personalizada de números aleatorios, lo que nos permite un mayor control sobre la generación de estos números.

## Ver también

- [Documentación oficial de Swift sobre generación de números aleatorios](https://developer.apple.com/documentation/swift/).
- [Tutorial en español sobre generación de números aleatorios en Swift](https://www.appstudio.dev/tutorial/generar-numeros-aleatorios-en-swift/).