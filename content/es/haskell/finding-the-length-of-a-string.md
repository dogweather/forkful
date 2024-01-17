---
title:                "Encontrar la longitud de una cadena"
html_title:           "Haskell: Encontrar la longitud de una cadena"
simple_title:         "Encontrar la longitud de una cadena"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Qué & Por qué?
En programación, encontrar la longitud de una cadena significa determinar la cantidad de caracteres que contiene esa cadena. Esto puede ser útil cuando se trabaja con datos de texto y se necesita saber cuántos caracteres hay en una cadena específica. 

Los programadores a menudo realizan esta tarea para validar el formato de entrada de datos, asegurarse de que no se exceda el límite de caracteres en una base de datos o simplemente para obtener información sobre la estructura de un texto.

## Cómo:
Para encontrar la longitud de una cadena en Haskell, podemos usar la función `length`. Esta función toma una cadena como argumento y devuelve un número entero que representa la longitud de la cadena. Veamos un ejemplo:

```Haskell
length "¡Hola mundo!" 
```

```
12
```

También podemos definir nuestra propia función para encontrar la longitud de una cadena. En este ejemplo, usaremos recursión para contar la cantidad de caracteres en una cadena:

```Haskell
findLength :: String -> Int
findLength [] = 0
findLength (x:xs) = 1 + findLength xs
```

En este código, la base de la recursión es una cadena vacía, que tiene una longitud de 0. Luego, cada vez que se llama a la función, se quita un carácter de la cadena y se agrega 1 al contador hasta que la cadena esté vacía. Veamos un ejemplo:

```Haskell
findLength "¡Hola mundo!"
```

```
12
```

## Deep Dive:
La función `length` en Haskell está definida como una función de tipo `list -> Int`, lo que significa que toma una lista como argumento y devuelve un número entero. En el caso de una cadena, Haskell la trata como una lista de caracteres, por lo que la función `length` funciona también para cadenas.

Una forma alternativa de encontrar la longitud de una cadena en Haskell es usando el operador `#` antes de la cadena, que devuelve la cantidad de elementos en la cadena, incluyendo sus paréntesis. Por ejemplo:

```Haskell
# "Hola"
```

```
5
```

## See Also:
- [Documentación oficial de Haskell](https://www.haskell.org/documentation/)
- [Tutorial de Haskell para principiantes](https://www.haskell.org/tutorial/index.html) 
- [Ejercicios de Haskell para practicar](https://exercism.io/tracks/haskell)