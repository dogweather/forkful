---
title:    "Haskell: Uniendo cadenas de texto"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## ¿Por qué concatenar cadenas en Haskell?
La concatenación de cadenas es una operación básica en cualquier lenguaje de programación que permite unir dos o más cadenas de texto en una sola. En Haskell, esta operación se realiza con la función `++` y es útil en muchas situaciones, como la creación de una salida de texto personalizada o la manipulación de datos.

## Cómo hacerlo
Para concatenar cadenas en Haskell, simplemente debes utilizar la función `++` entre las cadenas que desees unir. Por ejemplo:

```Haskell
nombre = "Juan"
apellido = "Pérez"

nombreCompleto = nombre ++ " " ++ apellido
```
La variable `nombreCompleto` ahora contendrá "Juan Pérez", ya que hemos concatenado la cadena `apellido` a la cadena `nombre` separadas por un espacio.

Otra forma de concatenar cadenas es utilizando la función `concat`, que toma una lista de cadenas y las concatena en una sola. Por ejemplo:

```Haskell
nombres = ["Juan", "Pablo", "José"]

listaNombres = concat nombres
```
La variable `listaNombres` ahora contendrá "JuanPabloJosé".

## Profundizando
En Haskell, las cadenas son simplemente listas de caracteres, por lo que la concatenación de cadenas es equivalente a la concatenación de listas. Esto significa que se pueden utilizar todas las funciones y operaciones de listas en cadenas, como `map`, `filter`, `foldr`, entre otras.

Además, al ser Haskell un lenguaje funcional, la concatenación de cadenas es una operación inmutable, lo que significa que no se modifican las cadenas originales al realizar la concatenación. En cambio, se crea una nueva cadena que contiene los caracteres de ambas.

En algunas situaciones, también puede ser útil utilizar la función `intercalate` que toma una cadena y una lista de cadenas y agrega la cadena entre cada elemento de la lista. Por ejemplo:

```Haskell
nombres = ["Juan", "Pablo", "José"]
saludo = "Hola"

saludoCompleto = intercalate saludo nombres
```
La variable `saludoCompleto` ahora contendrá "Juan Hola Pablo Hola José".

## Ver también
- [Documentación oficial de Haskell](https://www.haskell.org)
- [Tutorial de concatenación de cadenas en Haskell](https://www.tutorialspoint.com/haskell/haskell_string_concatenation.htm)
- [Ejemplos de concatenación de cadenas en Haskell](https://www.geeksforgeeks.org/concatenation-of-two-strings-in-haskell/)