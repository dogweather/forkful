---
title:                "Haskell: Uniendo cadenas"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Por qué es importante concatenar cadenas?
Al igual que en cualquier otro lenguaje de programación, en Haskell a menudo necesitamos combinar múltiples cadenas de texto juntas en una sola. Esto es especialmente útil cuando queremos mostrar información al usuario o crear mensajes personalizados. Además, la concatenación de cadenas nos permite crear programas más dinámicos y flexibles.

## Cómo hacerlo
En Haskell, podemos concatenar cadenas utilizando el operador `++` o la función `concat`. Veamos algunos ejemplos utilizando ambos métodos:

```Haskell
-- Utilizando el operador ++
concatenar = "Hola " ++ "mundo!"
-- Output: "Hola mundo!"

-- Utilizando la función concat con una lista de cadenas
concatenarLista = concat ["Me ", "gusta ", " programar."]
-- Output: "Me gusta programar."
```

## Profundizando
En Haskell, las cadenas de texto son en realidad listas de caracteres, por lo que podemos usar muchas funciones y métodos de listas con ellas. Por ejemplo, podemos utilizar la función `words` para dividir una cadena en una lista de palabras, y luego utilizar la función `unwords` para unir esa lista nuevamente en una sola cadena. Veamos un ejemplo:

```Haskell
cadena = "Este es un ejemplo de una cadena de texto."
palabras = words cadena
-- Output: ["Este", "es", "un", "ejemplo", "de", "una", "cadena", "de", "texto."]

cadenaNueva = unwords palabras
-- Output: "Este es un ejemplo de una cadena de texto."
```

## Ver también
- [Documentación oficial sobre cadenas en Haskell](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.14.0.0/Data-String.html)
- [Tutorial sobre cadenas en Haskell](https://www.tutorialspoint.com/haskell/haskell_strings.htm)
- [Explicación detallada sobre listas en Haskell](https://www.fpcomplete.com/haskell/tutorial/list/)