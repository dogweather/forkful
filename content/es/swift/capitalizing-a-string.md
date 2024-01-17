---
title:                "Capitalizando una cadena"
html_title:           "Swift: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

¡Hola lectores! ¿Estáis listos para aprender una forma sencilla de mejorar vuestro código Swift? En este artículo, vamos a hablar sobre cómo capitalizar una cadena de texto y por qué los programadores lo hacen. ¡Vamos al lío!

## ¿Qué es y por qué hacerlo?

Capitalizar una cadena de texto en Swift simplemente significa convertir la primera letra de cada palabra en mayúscula. Por ejemplo, "hola mundo" se convertiría en "Hola Mundo". ¿Por qué es importante? Porque es una forma de mejorar la legibilidad de nuestro código y facilitar su comprensión. Además, en algunos casos, puede ser un requisito para el correcto funcionamiento de nuestro programa.

## Cómo hacerlo:

Para capitalizar una cadena de texto en Swift, podemos usar el método `capitalized` de la clase `String`. Echemos un vistazo a un ejemplo:

```Swift
let myString = "hola mundo"
print(myString.capitalized)

// Output: Hola Mundo
```

También podemos especificar un local para asegurarnos de que se capitalicen correctamente las letras con acentos. Por ejemplo:

```Swift
let myString = "más allá"
print(myString.capitalized(with: Locale(identifier: "es_ES")))

// Output: Más Allá
```

¡Genial! Ahora ya sabemos cómo capitalizar una cadena de texto en Swift.

## Profundizando un poco más:

Ahora que sabemos cómo hacerlo, es posible que os preguntéis por qué no usamos simplemente el método `uppercase` o `lowercase` para convertir todo el texto en mayúsculas o minúsculas. La respuesta es que, a veces, queremos preservar ciertas letras en minúsculas o mayúsculas para una mejor comprensión del texto. Además, recordad que para algunos idiomas, como el español, la capitalización es importante.

Otra forma de capitalizar una cadena de texto en Swift es usando un ciclo `for` para recorrer la cadena y convertir la primera letra de cada palabra manualmente. Sin embargo, esto puede ser tedioso y propenso a errores, por lo que es mejor seguir utilizando el método `capitalized`.

## Véase también:

- [Documentación oficial de Swift sobre el método `capitalized`](https://developer.apple.com/documentation/foundation/nsstring/1413495-capitalized)
- [Una explicación más detallada sobre el uso de locales en Swift](https://learnappmaking.com/localization-swift-5-how-to/)
- [Un artículo sobre cómo mejorar la legibilidad del código en Swift](https://medium.com/@alanarvelo/improve-your-code-legibility-in-swift-52c60e0ab17f)

¡Eso es todo por hoy! Esperamos que este artículo os haya sido útil y que os animéis a seguir mejorando vuestros skills en Swift. ¡Feliz coding!