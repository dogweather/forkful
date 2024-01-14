---
title:                "Swift: Buscando y reemplazando texto"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué

Buscar y reemplazar texto es una tarea común en la programación. Nos permite cambiar rápidamente partes específicas de nuestro código sin tener que cambiar cada instancia manualmente. Aprender a hacerlo eficientemente puede ahorrarnos tiempo y simplificar nuestro flujo de trabajo de programación.

## Cómo hacerlo

Para buscar y reemplazar texto en Swift, utilizamos el método `replacingOccurrences(of:with:)` en un `String`. Este método toma dos argumentos: el texto que queremos reemplazar y el texto con el que lo queremos reemplazar.

Veamos un ejemplo:

```Swift
var message = "Hola mundo!"
var newMessage = message.replacingOccurrences(of: "mundo", with: "amigos")
print(newMessage) // Salida: Hola amigos!
```

En este ejemplo, hemos reemplazado la palabra "mundo" con "amigos" en la variable `message`. Podemos ver que la salida ahora es "Hola amigos!" en lugar de "Hola mundo!".

También podemos usar este método para reemplazar múltiples instancias de un texto en una sola vez. Simplemente agregamos la opción `replaceAll` al final del método. Veamos otro ejemplo:

```Swift
let fruits = "manzana, banana, naranja, manzana, pera"
let newFruits = fruits.replacingOccurrences(of: "manzana", with: "fresa", options: .replaceAll)
print(newFruits) // Salida: fresa, banana, naranja, fresa, pera
```

En este caso, hemos reemplazado todas las instancias de "manzana" con "fresa" en la cadena `fruits`.

## Profundizando

Además de los dos argumentos principales, el método `replacingOccurrences(of:with:)` también tiene una opción adicional llamada `range`. Esta opción nos permite especificar en qué parte de la cadena queremos buscar y reemplazar.

Por defecto, esta opción está establecida en `nil`, lo que significa que reemplazará todas las instancias. Pero si queremos ser más específicos, podemos proporcionar un rango de índices de caracteres para indicar dónde queremos buscar.

Veamos un ejemplo:

```Swift
let sentence = "Mi comida favorita es pizza"
let newSentence = sentence.replacingOccurrences(of: "pizza", with: "sushi", options: .withoutOverlapping, range: 3..<9)
print(newSentence) // Salida:Mi comida favorita es sushi
```

Aquí, hemos pasado un rango de índices que excluye la palabra "Mi" al principio y "favorita" al final, por lo que solo se reemplazará "pizza" en el medio de la cadena.

## Ver también

- Documentación oficial de Swift sobre el método `replacingOccurrences(of:with:)`: https://developer.apple.com/documentation/foundation/nsstring/1412839-replacingoccurrences