---
title:                "Swift: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué
Concatenar strings es una tarea común en la programación, especialmente en Swift. Al unir diferentes strings, podemos crear mensajes personalizados, agregar variables a strings y construir contenido dinámico para nuestras aplicaciones.

## Cómo hacerlo
Para concatenar strings en Swift, podemos usar el operador "+" o el método "append()". Por ejemplo:

```Swift
let nombre = "Juan"
let edad = 28
let mensaje = "¡Hola! Me llamo " + nombre + " y tengo " + String(edad) + " años."
print(mensaje)
// Output: ¡Hola! Me llamo Juan y tengo 28 años. 
```
También podemos utilizar el método "join()" para unir múltiples strings en un solo string. Por ejemplo:

```Swift
let nombres = ["Ana", "Carlos", "Sofía"]
let mensaje = "¡Hola a todos! Mi nombre es " + nombres.joined(separator: ", ") + "."
print(mensaje)
// Output: ¡Hola a todos! Mi nombre es Ana, Carlos, Sofía.
```

## Profundizando
Cuando concatenamos strings en Swift, hay algunas cosas a tener en cuenta. Primero, es importante tener en cuenta los tipos de datos. Si intentamos unir un string con un entero, Swift nos dará un error. Por eso, debemos asegurarnos de convertir los datos en el tipo correcto antes de concatenarlos.

Además, al utilizar el método "join()", es importante recordar que necesitamos un array de strings. Si intentamos usar un array de integers, por ejemplo, obtendremos un error.

Una forma sencilla de convertir un número en un string en Swift es usando el método "String()". Este método convierte cualquier valor numérico en un string.

## Ver también
- Documentación oficial de Swift sobre strings: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html
- Tutorial sobre concatenación de strings en Swift: https://www.tutorialspoint.com/swift/swift_string_concatenation.htm
- Ejemplos prácticos de concatenación de strings en Swift: https://www.9lessons.info/2017/01/concatenation-of-strings-in-swift-3.html