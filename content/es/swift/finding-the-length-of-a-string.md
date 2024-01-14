---
title:                "Swift: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En la programación Swift, conocer la longitud de una cadena puede ser útil para realizar diversas tareas, como validar entradas de usuario o manipular cadenas para mostrar información específica. Afortunadamente, encontrar la longitud de una cadena es un proceso sencillo que te permitirá mejorar tus habilidades de programación en Swift.

## Cómo hacerlo

Para encontrar la longitud de una cadena en Swift, simplemente utilizamos el método `count` en la cadena. A continuación se muestra un ejemplo de cómo encontrar la longitud de la cadena "Hola mundo":

```Swift
let cadena = "Hola mundo"
print(cadena.count)
// Output: 10
```

En este ejemplo, el método `count` devuelve el número de caracteres de la cadena, incluyendo los espacios en blanco.

Otra forma de encontrar la longitud de una cadena es mediante el uso del operador `count` en la cadena, como se muestra a continuación:

```Swift
let cadena = "¡Hola!"
print(cadena.count)
// Output: 6
```

Como se puede observar, el método `count` también funciona en cadenas que incluyen caracteres especiales.

## Profundizando

Aunque el método `count` es el más comúnmente utilizado para encontrar la longitud de una cadena, también existen otras opciones como el método `characters` y el operador `isEmpty`.

El método `characters` devuelve una colección de caracteres de la cadena, lo que nos permite realizar operaciones adicionales. A continuación se muestra un ejemplo de cómo utilizar este método:

```Swift
let cadena = "Hola"
print(cadena.characters.count)
// Output: 4
```

Por otro lado, el operador `isEmpty` nos permite comprobar si la cadena está vacía o no. En el siguiente ejemplo, utilizamos este operador para imprimir un mensaje si la cadena está vacía:

```Swift
let cadena = ""
if cadena.isEmpty {
    print("La cadena está vacía.")
} else {
    print("La longitud de la cadena es: \(cadena.count)")
}
// Output: La cadena está vacía.
```

Recuerda que siempre puedes consultar la documentación oficial de Swift para obtener más información sobre estos y otros métodos para encontrar la longitud de una cadena.

## Ver también

- [Documentación oficial de Swift](https://developer.apple.com/documentation/swift)
- [Tutorial de Swift para principiantes](https://www.raywenderlich.com/5996-swift-tutorial-for-beginners)
- [Ejemplos de código para Swift](https://github.com/apple/swift/tree/master/samples)