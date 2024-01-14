---
title:                "Swift: Concatenando cadenas"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Por qué concatenar cadenas en Swift?

Concatenar cadenas es una práctica común en la programación, especialmente en Swift. Permite combinar diferentes cadenas para crear una nueva y usarla en nuestras aplicaciones. Esta técnica es útil para crear títulos, mensajes de alerta o cualquier texto dinámico en nuestro código.

## Cómo hacerlo

En Swift, podemos concatenar cadenas de varias maneras. La forma más básica es usando el operador `+` para unir dos cadenas. Por ejemplo:

```Swift
let firstName = "Juan"
let lastName = "Pérez"
let greeting = "¡Hola " + firstName + " " + lastName + "!"
print(greeting)
// Output: ¡Hola Juan Pérez!
```

También podemos utilizar la función `String(format:...)` para concatenar cadenas con formato. Esta función toma dos argumentos: la plantilla de formato y los valores a insertar en la cadena. Por ejemplo:

```Swift
let num1 = 5
let num2 = 10
let result = String(format: "La suma de %d y %d es %d", num1, num2, num1+num2)
print(result)
// Output: La suma de 5 y 10 es 15
```

Otra forma de concatenar cadenas en Swift es utilizando la función `joined(separator:)` en un array de cadenas. Esta función nos permite unir todas las cadenas con un separador específico. Por ejemplo:

```Swift
let fruits = ["manzana", "naranja", "kiwi"]
let fruitsString = fruits.joined(separator: ", ")
print("Me gustan las frutas: " + fruitsString)
// Output: Me gustan las frutas: manzana, naranja, kiwi
```

## Profundizando

Un aspecto importante al concatenar cadenas es el rendimiento. En Swift, se recomienda utilizar la estructura `String` en lugar de la clase `NSString` para mejorar el rendimiento de nuestro código. Además, también debemos tener en cuenta que el operador `+` crea una nueva cadena en la memoria cada vez que se usa, por lo que en casos de concatenar varias cadenas juntas, es más eficiente utilizar la función `appending(_:)` en una sola cadena. Por ejemplo:

```Swift
let city = "Barcelona"
let country = "España"
let address = city.appending(", " + country)
print(address)
// Output: Barcelona, España
```

## Ver también

Para más información sobre concatenar cadenas en Swift, puedes visitar los siguientes enlaces:

- Documentación de Apple sobre la estructura `String`: https://developer.apple.com/documentation/swift/string
- Tutorial de Hacking with Swift sobre la concatenación de cadenas: https://www.hackingwithswift.com/read/2/2/concatenating-strings
- Artículo de Ray Wenderlich sobre el rendimiento de cadenas en Swift: https://www.raywenderlich.com/123907/swift-string-performance/