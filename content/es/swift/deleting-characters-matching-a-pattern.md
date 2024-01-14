---
title:    "Swift: Borrando caracteres que coinciden con un patrón"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# ¿Por qué deberías eliminar caracteres que coincidan con un patrón en Swift?

Eliminar caracteres que coincidan con un patrón puede ser una tarea útil en la programación, especialmente si estás trabajando con cadenas de texto y necesitas extraer cierta información específica. Por ejemplo, si tienes un conjunto de datos y solo quieres obtener los números, puedes utilizar esta técnica para eliminar todos los caracteres que no sean números.

## Cómo hacerlo

Para eliminar caracteres que coincidan con un patrón en Swift, puedes utilizar la función `replacingOccurrences` de la clase `String`.

```Swift
let texto = "¡Sólo quiero los números 123!"
let numeros = texto.replacingOccurrences(of: "[^0-9]", with: "", options: .regularExpression)
print(numeros)
```
La salida de este código sería "123", ya que hemos eliminado todos los caracteres que no sean números utilizando una expresión regular.

## Profundizando

Para comprender mejor cómo funciona esta técnica, es importante entender qué es una expresión regular. Una expresión regular es una secuencia de caracteres que define un patrón de búsqueda. En el ejemplo anterior, `[0-9]` es una expresión regular que indica cualquier carácter numérico y `^` es un operador de negación que indica que queremos eliminar todos los caracteres que no sean números.

También puedes utilizar otras opciones con la función `replacingOccurrences`, como `caseInsensitive` para ignorar las mayúsculas y minúsculas y `anchored` para asegurarte de que solo se eliminan los caracteres al comienzo de la cadena.

# Ver también

Aquí hay algunos recursos adicionales que pueden ser útiles para aprender más sobre cómo eliminar caracteres que coincidan con un patrón en Swift:

- [Documentación oficial de Apple sobre la función replacingOccurrences](https://developer.apple.com/documentation/foundation/string/1789849-replacingoccurrences)
- [Tutorial sobre expresiones regulares en Swift](https://www.raywenderlich.com/167729/swift-regular-expressions-tutorial)