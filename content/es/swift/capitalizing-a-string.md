---
title:                "Swift: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué capitalizar una cadena en Swift?

En algunos casos, puede ser necesario capitalizar una cadena en Swift para hacerla más legible o estandarizar su formato. Esto es especialmente útil en aplicaciones que requieren una entrada de datos precisa y consistente.

## ¿Cómo hacerlo?

Para capitalizar una cadena en Swift, hay varias opciones disponibles. Una forma sencilla es utilizando el método `capitalized` en una variable de tipo `String`. Por ejemplo:

```Swift
let cadena = "este es un ejemplo"
print(cadena.capitalized)
```
**Salida:** Este Es Un Ejemplo

También se puede utilizar el método `uppercased` para convertir todos los caracteres de la cadena a mayúsculas, o `lowercased` para convertirlos a minúsculas.

```Swift
print(cadena.uppercased())
```
**Salida:** ESTE ES UN EJEMPLO

```Swift
print(cadena.lowercased())
```
**Salida:** este es un ejemplo

Otra opción es utilizar el método `prefix` para capitalizar solo la primera letra de la cadena:

```Swift
print(cadena.prefix(1).capitalized + cadena.dropFirst())
```
**Salida:** Ese es un ejemplo

## Profundizando en la capitalización de cadenas

En Swift, la capitalización de cadenas también puede variar según el idioma y la región. Por ejemplo, si se utiliza `capitalized` en una cadena en español, se respetarán las reglas de capitalización del español, donde se debe respetar la mayúscula en la primera palabra y en los nombres propios.

Por otro lado, si se desea una capitalización personalizada, se puede utilizar el método `capitalized(with:)` y especificar un `Locale` que indique cómo se debe capitalizar la cadena. Por ejemplo:

```Swift
let cadena = "este es un ejemplo"
let locale = Locale(identifier: "es_MX")
print(cadena.capitalized(with: locale))
```
**Salida:** Este es un Ejemplo

## Ver también

- [Documentación oficial de Apple sobre la capitalización de cadenas en Swift](https://developer.apple.com/documentation/swift/string/2960994-capitalized)
- [Tutorial de Hacking with Swift sobre capitalización de cadenas en Swift](https://www.hackingwithswift.com/example-code/strings/how-to-capitalize-the-first-letter-of-a-string)