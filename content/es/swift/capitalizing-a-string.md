---
title:                "Swift: Capitalizando una cadena"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué
Al escribir código en Swift, es común encontrarse con la necesidad de capitalizar una cadena de texto. Esto puede ser para mejorar la legibilidad o para cumplir con ciertas convenciones de estilo. En este artículo, veremos cómo capitalizar una cadena en Swift de manera eficiente.

## Cómo hacerlo
Para capitalizar una cadena en Swift, podemos utilizar el método `capitalized` en una instancia de `String`. Este método tomará la primera letra de cada palabra en la cadena y la convertirá a mayúscula, manteniendo el resto en minúscula. Además, también se encargará de mantener las letras acentuadas del idioma español en su forma adecuada.

````Swift
let myString = "hola, ¿cómo estás?"

print(myString.capitalized)

// Salida: Hola, ¿Cómo Estás?
````

Si queremos capitalizar solo la primera letra de la cadena y mantener el resto en minúscula, podemos utilizar el método `capitalizedFirst` de la siguiente manera:

````Swift
let myString = "hola, ¿cómo estás?"

print(myString.capitalizedFirst)

// Salida: Hola, ¿cómo estás?
````

También podemos capitalizar solo la primera palabra de la cadena utilizando el método `capitalizedFirst` combinado con el método `lowercased`:

````Swift
let myString = "hola, ¿cómo estás?"

print(myString.lowercased().capitalizedFirst)

// Salida: Hola, ¿cómo estás?
````

## Profundizando
Además de los métodos mencionados anteriormente, también podemos capitalizar una cadena utilizando las funciones `uppercased` y `lowercased`. Estas funciones convierten todas las letras de una cadena a mayúscula o minúscula respectivamente.

Otra forma de capitalizar una cadena es utilizando una extensión de Swift que escribamos nosotros mismos. De esta manera, podemos crear una función que se adapte a nuestras necesidades específicas, como por ejemplo, capitalizar solo la primera letra de cada palabra en una cadena con determinados patrones.

## Ver también
- [Documentación oficial de Apple sobre el tipo de datos String](https://developer.apple.com/documentation/swift/string)
- [Extensión de Swift para capitalizar una cadena](https://medium.com/@mayankmohak/capitalizing-last-name-in-swift-without-terrorism-31cb5b0b5eb8)
- [Tutorial de Hacking with Swift sobre capitalizar cadenas](https://www.hackingwithswift.com/example-code/strings/how-to-capitalize-the-first-letter-of-a-string)