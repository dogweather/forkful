---
title:                "Capitalizando una cadena de texto"
date:                  2024-01-19
html_title:           "Arduino: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Capitalizar una cadena en Swift significa convertir la primera letra de cada palabra a mayúscula. Los programadores lo hacen para asegurarse de que los títulos, nombres propios o cualquier texto que requiera un formato estándar, se vean consistentes y profesionales.

## Cómo hacerlo:

En Swift, puedes capitalizar strings fácilmente utilizando el método `capitalized`. Aquí te muestro cómo:

```Swift
let phrase = "bienvenidos al mundo de Swift"
let capitalizedPhrase = phrase.capitalized
print(capitalizedPhrase)
```

Salida de muestra:

```
Bienvenidos Al Mundo De Swift
```

Si solo quieres la primera letra de la cadena en mayúscula, usa esto:

```Swift
let firstLetterCapitalized = phrase.prefix(1).uppercased() + phrase.dropFirst()
print(firstLetterCapitalized)
```

Salida de muestra:

```
Bienvenidos al mundo de Swift
```

## Inmersión Profunda:

Históricamente, los métodos para cambiar la capitalización de cadenas tienen sus raíces en la necesidad de la tipografía y la gramática adecuada. En la era digital, la consistencia visual en las interfaces y en los datos se ha vuelto esencial, y funciones como `capitalized` simplifican este proceso.

Swift proporciona una implementación clara con `capitalized`, pero ten en cuenta que este método es específico para el lenguaje inglés y puede no funcionar correctamente con otros idiomas o caracteres especiales. En contraste, las bibliotecas de internacionalización pueden ofrecer alternativas que respeten las reglas de capitalización específicas de cada idioma. 

La capitalización también afecta a la comparación de cadenas si uno espera que sea insensible a mayúsculas y minúsculas. Para esos casos, Swift ofrece métodos como `caseInsensitiveCompare`.

En cuanto a la implementación, `capitalized` es parte de la extensión de String y utiliza las reglas de Unicode para transformar la primera letra de cada palabra en mayúscula y el resto en minúscula.

## Ver También:

- [Documentación oficial de Swift sobre Strings](https://developer.apple.com/documentation/swift/string)
- [Guía de Unicode sobre la capitalización de palabras](http://unicode.org/faq/casemap_charprop.html#7)
- [Tutorial de Swift sobre la manipulación de strings](https://www.hackingwithswift.com/sixty/5/2/uppercased-and-lowercased)
