---
title:                "Concatenación de cadenas"
html_title:           "Haskell: Concatenación de cadenas"
simple_title:         "Concatenación de cadenas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Por qué?

¿Alguna vez has querido unir varias palabras o frases en un solo texto? La concatenación de cadenas es una herramienta útil en programación para combinar piezas de texto de manera eficiente, ya sea para mostrar información al usuario o para construir cadenas más complejas.


## Cómo hacerlo

Para unir cadenas en Haskell, utilizamos el operador `++`. Por ejemplo, si tenemos dos cadenas, "Hola" y "mundo", podemos unirlas de la siguiente manera:

```Haskell
"Hola" ++ "mundo" -- resultado: "Hola mundo"
```

También podemos usar el operador varias veces para unir más de dos cadenas:

```Haskell
"Mi" ++ " " ++ "nombre" ++ " " ++ "es" ++ " " ++ "Juan" -- resultado: "Mi nombre es Juan"
```

Incluso podemos unir cadenas y caracteres:

```Haskell
"¡Hola," ++ ' ' ++ "bienvenido!" -- resultado: "¡Hola, bienvenido!"
```

Otra forma de concatenar cadenas es utilizando la función `concat` en lugar del operador `++`:

```Haskell
concat ["Me", "gusta", "Haskell"] -- resultado: "Me gusta Haskell"
```

## Profundizando

En Haskell, las cadenas son simplemente listas de caracteres. Por lo tanto, el operador `++` funciona de manera similar a la función `++` para unir listas.

Además, existen otras funciones útiles para la manipulación de cadenas en Haskell, como `length` para obtener la longitud de una cadena, `take` y `drop` para obtener subcadenas, y `reverse` para invertir una cadena.

En resumen, la concatenación de cadenas es una herramienta importante en Haskell que nos permite construir cadenas de manera eficiente y flexible. ¡Experimenta con diferentes formas de unir cadenas y descubre todas las posibilidades que ofrece esta función!

## Ver también

- [Documentación oficial de Haskell sobre cadenas](https://www.haskell.org/tutorial/strings.html)
- [Tutorial de Haskell en español](https://github.com/Next-Lvl-Girls/pendientes-haskell-praga)
- [Ejemplos de concatenación de cadenas en vivo en Haskell](https://repl.it/@NextLvlGirls/Concatenar-cadenas-en-Haskell)