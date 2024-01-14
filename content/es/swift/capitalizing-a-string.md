---
title:    "Swift: Capitalizando una cadena"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué
En esta publicación, hablaremos sobre cómo capitalizar una cadena en Swift. Si eres nuevo en el mundo de la programación o si tienes curiosidad acerca de por qué alguien querría hacer esto, ¡sigue leyendo!

## Cómo hacerlo
Para capitalizar una cadena en Swift, puedes utilizar la función incorporada `capitalized` en la clase `String`. Aquí hay un ejemplo de cómo hacerlo:

```Swift
let cadena = "hola mundo"
let cadenaCapitalizada = cadena.capitalized
print(cadenaCapitalizada)
```
La salida de este código será "Hola Mundo".

También puedes usar la función `capitalizingFirstLetter()` si solo quieres capitalizar la primera letra de la cadena. Por ejemplo:

```Swift
let nombre = "carlos"
let nombreCapitalizado = nombre.capitalizingFirstLetter()
print(nombreCapitalizado)
```
La salida de este código será "Carlos".

## Profundizando
Sabemos que la función `capitalized` capitaliza la primera letra de cada palabra en la cadena. Pero, ¿cómo funciona exactamente?

En Swift, las cadenas se representan como una serie de caracteres. La función `capitalized` utiliza la propiedad `uppercaseFirst` en cada palabra para convertir su primera letra en mayúscula. Luego, utiliza la función `lowercased` para convertir el resto de las letras en minúsculas. De esta manera, la cadena queda capitalizada.

## Ver también
- [Documentación oficial de Swift sobre capitalizing strings](https://developer.apple.com/documentation/swift/string/2947153-capitalized)
- [Ejemplos de capitalización de cadenas en Swift](https://www.hackingwithswift.com/example-code/strings/how-to-capitalize-the-first-letter-of-a-string)
- [Simples trucos de Swift: cómo capitalizar una palabra](https://www.hackingwithswift.com/example-code/strings/how-to-capitalize-the-first-letter-of-a-word)