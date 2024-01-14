---
title:    "Swift: Encontrar la longitud de una cadena"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por qué
A veces, en la programación, es necesario saber cuántos caracteres tiene una cadena de texto. Ya sea para validar una entrada de usuario o para realizar alguna operación en base a la longitud de la cadena, saber la cantidad de caracteres es una habilidad esencial en programación.

## Cómo hacerlo
Para encontrar la longitud de una cadena de texto en Swift, podemos utilizar la propiedad `count` que está disponible en todas las cadenas. Esta propiedad nos devuelve la cantidad de caracteres de la cadena como un número entero.

```Swift
let cadena = "¡Hola, mundo!"
print(cadena.count) // Output: 13
```

También podemos utilizar la función `characters.count` en versiones anteriores de Swift.

```Swift
let cadena = "¡Hola, mundo!"
print(cadena.characters.count) // Output: 13
```

Si queremos encontrar la longitud de una cadena que contiene emojis o caracteres Unicode, es importante tener en cuenta que estos caracteres pueden tener más de un código Unicode. Por lo tanto, la función `count` o `characters.count` nos devolverá la cantidad de códigos Unicode, no la cantidad de caracteres visibles.

## Profundizando
En Swift, las cadenas de texto son almacenadas como una colección de caracteres Unicode. Esto significa que cada caracter tiene un valor numérico asociado, conocido como código Unicode. Al utilizar la propiedad `count` o `characters.count`, estamos contando la cantidad de códigos Unicode dentro de la cadena.

Si queremos obtener la cantidad de caracteres visibles en una cadena que contiene emojis o caracteres Unicode, podemos utilizar la función `countByEnumerating(collecion: Collection)` de la siguiente manera:

```Swift
let cadena = "¡Hola, 🌎!"
var contador = 0
cadena.countByEnumeratingCharacters(in: cadena.characters) { (caracter, _, _) in
    contador += 1
}
print(contador) // Output: 7
```

En este ejemplo, la función `countByEnumeratingCharacters` itera sobre cada caracter en la cadena y aumenta el contador en uno por cada caracter. Esto nos devuelve el resultado deseado de 7 caracteres visibles en lugar de 9 códigos Unicode.

## Ver también
- [Documentación oficial de Swift sobre propiedades de cadenas](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID299)
- [Artículo sobre códigos Unicode en Swift](https://www.globalnerdy.com/2019/02/11/understanding-unicode-in-swift/)

¡Eso es todo! ¡Ahora puedes encontrar fácilmente la longitud de cualquier cadena de texto en Swift!