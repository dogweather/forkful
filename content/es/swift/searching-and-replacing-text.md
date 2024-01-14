---
title:                "Swift: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

##Por qué

La búsqueda y reemplazo de texto es una herramienta esencial para cualquier programador, ya que nos permite hacer cambios rápidos y eficientes en nuestro código. Ya sea que necesitemos cambiar una variable en todo nuestro proyecto o corregir un error ortográfico en múltiples líneas de código, la función de búsqueda y reemplazo nos ahorra tiempo y esfuerzo.

##Cómo hacerlo

En Swift, podemos utilizar la función `replacingOccurrences(of:with:)` para realizar la búsqueda y reemplazo de texto. Esta función toma dos parámetros: el texto que queremos reemplazar y el texto que queremos poner en su lugar. Por ejemplo:

```Swift
let message = "Hola mundo"
let newMessage = message.replacingOccurrences(of: "mundo", with: "amigos")
```
El resultado de `newMessage` sería "Hola amigos". Como puedes ver, la función reemplaza todas las apariciones del texto "mundo" en `message` con el texto "amigos".

También podemos utilizar la función `replacingCharacters(in:with:)` para reemplazar un carácter específico dentro de una cadena. Esta función toma dos parámetros: el rango de caracteres que queremos reemplazar y el texto que queremos poner en su lugar. Por ejemplo:

```Swift
let name = "Juan"
let newName = name.replacingCharacters(in: Range(NSRange(location: 3, length: 1), in: name)!, with: "ito")
```
El resultado de `newName` sería "Juanito". Aquí, hemos utilizado el rango `NSRange(location: 3, length: 1)` para especificar que queremos reemplazar el cuarto carácter (indice 3) de `name` por el texto "ito".

##Profundizando

Las funciones `replacingOccurrences(of:with:)` y `replacingCharacters(in:with:)` son solo algunas de las muchas formas en que podemos realizar la búsqueda y reemplazo de texto en Swift. También podemos utilizar expresiones regulares para buscar patrones específicos dentro de una cadena y reemplazarlos con diferentes textos. Esto puede ser muy útil en situaciones como validar correos electrónicos o números de teléfono en un formulario.

Además, existen algunas librerías de terceros que ofrecen aún más funcionalidades y opciones para la búsqueda y reemplazo de texto en Swift. Así que, si quieres profundizar aún más en este tema, asegúrate de explorar estas opciones y encontrar la que mejor se adapte a tus necesidades.

##Ver también

- Documentación oficial de Apple sobre `replacingOccurrences(of:with:)`: https://developer.apple.com/documentation/foundation/nsstring/1413955-replacingoccurrences
- Documentación oficial de Apple sobre `replacingCharacters(in:with:)`: https://developer.apple.com/documentation/foundation/nsstring/1414309-replacingcharacters
- Expresiones regulares en Swift: https://www.raywenderlich.com/86205/nsregularexpression-swift-tutorial
- Gráfico de referencia rápida para expresiones regulares en Swift: https://www.iphone-tutorial.net/2018/07/06/expresiones-regulares-en-swift/