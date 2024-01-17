---
title:                "Usando expresiones regulares"
html_title:           "Swift: Usando expresiones regulares"
simple_title:         "Usando expresiones regulares"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
 Las expresiones regulares son patrones de búsqueda que se utilizan en la programación para encontrar y manipular texto dentro de una cadena de caracteres. Los programadores usan expresiones regulares porque son una herramienta poderosa y eficiente para realizar operaciones de búsqueda y manipulación en grandes cantidades de datos de forma precisa y eficiente.

## Cómo:
Aquí hay un ejemplo de cómo usar expresiones regulares en Swift para obtener una lista de todas las direcciones de correo electrónico en una cadena de texto:

```Swift
let text = "Hola, mi correo electrónico es john@example.com y mi otro correo es jane@example.com"

let regex = try! NSRegularExpression(pattern: "\\b([a-zA-Z0-9_.-]+)@([a-zA-Z0-9_.-]+)\\.(com|net|org)\\b", options: [])
let matches = regex.matches(in: text, options: [], range: NSRange(location: 0, length: text.utf16.count))

let emailAddresses = matches.map {
    String(text[Range($0.range, in: text)!])
}

print(emailAddresses)
```
Este código utiliza la clase `NSRegularExpression` de Swift para definir un patrón de búsqueda que busca direcciones de correo electrónico válidas. Luego, utiliza el método `matches` para buscar todas las coincidencias en la cadena de texto y finalmente, se crea una lista de todas las direcciones de correo electrónico encontradas.

## Profundizando:
Las expresiones regulares han existido desde la década de 1950 y se han utilizado en una variedad de lenguajes de programación. Algunos lenguajes, como Perl, incluso tienen una sintaxis especial para trabajar con expresiones regulares. Sin embargo, Swift ha incorporado las expresiones regulares directamente en el lenguaje, lo que las hace aún más accesibles para los desarrolladores.

Alternativamente, los desarrolladores pueden usar otras técnicas para buscar y manipular texto, como el uso de métodos de cadena incorporados y bucles. Sin embargo, estas técnicas pueden resultar mucho más verbosas y menos eficientes que el uso de expresiones regulares.

La implementación de expresiones regulares en Swift es a través de la clase `NSRegularExpression`, que acepta un patrón de búsqueda y una cadena de texto para buscar. Los patrones de búsqueda se pueden personalizar para cumplir con las necesidades específicas del programador, lo que hace que las expresiones regulares sean una herramienta muy flexible.

## Ver también:
Puedes aprender más sobre expresiones regulares en la documentación oficial de Swift y explorando diferentes patrones de búsqueda en línea. También puedes utilizar aplicaciones como RegExr o RegExLab para probar y experimentar con tus propias expresiones regulares. ¡Diviértete buscando y manipulando texto con expresiones regulares en Swift!