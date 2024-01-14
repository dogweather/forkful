---
title:    "Swift: Convirtiendo una cadena a minúsculas"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por qué

En la programación, a veces tenemos que manipular cadenas de texto para que estén en un formato específico. Una de estas manipulaciones comunes es convertir una cadena de texto a minúsculas. Esto es útil para comparar y buscar cadenas de texto sin tener en cuenta las mayúsculas y minúsculas.

## Cómo hacerlo

En Swift, hay una función incorporada llamada `lowercased ()` que nos permite convertir una cadena de texto a minúsculas. Veamos un ejemplo de cómo usarlo:

```Swift
let frase = "¡Hola, Mundo!"
print(frase.lowercased())
```

El resultado de este código será `¡hola, mundo!`. Como se puede ver, todas las letras se han convertido a minúsculas. También podemos aplicar esta función a variables o constantes que contengan cadenas de texto. Por ejemplo:

```Swift
var nombre = "Juan"
print(nombre.lowercased())
```

En este caso, el resultado sería `juan`. 

## Profundizando

Además de la función `lowercased ()`, también podemos usar otros métodos para convertir cadenas de texto a minúsculas. Uno de ellos es el método `caseInsensitiveCompare ()`, que permite comparar dos cadenas de texto sin tener en cuenta las mayúsculas y minúsculas. Este método devuelve un tipo de dato `ComparisonResult` que puede ser usado para realizar una comparación lógica entre las cadenas. Por ejemplo:

```Swift
let palabraA = "casa"
let palabraB = "CASA"
if palabraA.caseInsensitiveCompare(palabraB) == .orderedSame {
    print("Las palabras son iguales")
} else {
    print("Las palabras son diferentes")
}
```

En este caso, el código imprimirá "Las palabras son iguales". También podemos usar el método `lowercased ()` dentro de esta comparación para asegurarnos de que ambas cadenas estén en minúsculas antes de hacer la comparación.

## Ver también

A continuación, te dejamos algunos enlaces útiles para aprender más sobre la manipulación de cadenas de texto en Swift:

- [Documentación de Apple sobre la manipulación de cadenas de texto](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Tutorial sobre conversión de cadenas de texto a minúsculas en Swift](https://www.hackingwithswift.com/example-code/strings/how-to-convert-a-string-to-lowercase)
- [Artículo sobre la importancia de manejar las mayúsculas y minúsculas en la programación](https://www.freecodecamp.org/news/why-coding-without-caps-lock/)

Esperamos que este artículo te haya ayudado a comprender cómo convertir cadenas de texto a minúsculas en Swift. ¡Continúa explorando y aprendiendo más sobre este lenguaje de programación!