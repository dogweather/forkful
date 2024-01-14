---
title:                "Swift: Eliminando caracteres que coincidan con un patrón"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Porqué

A menudo, al trabajar con cadenas de texto, es necesario eliminar ciertos caracteres que están presentes en un patrón específico. Esto puede ser por razones de formateo o por necesidad de limpiar los datos antes de usarlos. Afortunadamente, Swift nos ofrece una forma sencilla de hacerlo: eliminando caracteres que coinciden con un patrón dado.

# Cómo Hacerlo

Para eliminar caracteres que coinciden con un patrón en Swift, podemos utilizar la función `replacingOccurrences(of:with:)`. Esta función toma dos argumentos: el patrón que queremos buscar y el valor con el que queremos reemplazarlo. Por ejemplo, si queremos eliminar todas las vocales de una cadena de texto, podemos usar esta función de la siguiente manera:

```
let texto = "Hola, cómo estás?"
let textoSinVocales = texto.replacingOccurrences(of: "[aeiou]", with: "", options: .regularExpression)
print(textoSinVocales)
```
Este código imprimirá "Hl, cm stás?" ya que todas las vocales han sido eliminadas. 

# Profundizando

La función `replacingOccurrences(of:with:)` también acepta la opción `.regularExpression` que nos permite utilizar expresiones regulares para encontrar el patrón que queremos eliminar. Las expresiones regulares son patrones de búsqueda flexibles y potentes que nos permiten encontrar y manipular cadenas de texto de manera eficiente.

Además, esta función también tiene una variante que elimina el patrón de manera recursiva, es decir, si el patrón se encuentra dentro de la cadena de texto varias veces, todas las instancias del patrón serán eliminadas. Para utilizar esta variante, solo debemos agregar la opción `.replaceAll` al llamar a la función.

# Ver También

- Documentación oficial de Swift sobre la función `replacingOccurrences(of:with:)`: https://developer.apple.com/documentation/swift/string/2894561-replacingoccurrences
- Tutorial sobre expresiones regulares en Swift: https://www.raywenderlich.com/1584835-regular-expressions-in-swift-tutorial-getting-started
- Swift by Sundell: Eliminar todas las ocurrencias de un patrón en una cadena de texto: https://www.swiftbysundell.com/articles/removing-all-occurences-of-a-pattern-from-a-string/