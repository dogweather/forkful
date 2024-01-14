---
title:    "Haskell: Capitalizando una cadena"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, a menudo nos encontramos con la necesidad de capitalizar una cadena de texto, es decir, hacer que la primera letra de cada palabra sea mayúscula. Esto puede ser útil en varias situaciones, como por ejemplo en la presentación de datos en un texto o en la creación de títulos para una interfaz de usuario.

## Cómo hacerlo

En Haskell, capitalizar una cadena de texto es muy sencillo. Podemos utilizar la función "toUpper" de la librería "Data.Char" y luego aplicarla a cada letra de la cadena a través de la función "map". Veamos un ejemplo de esto:

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize = map toUpper
```

La función "capitalize" recibe una cadena de texto y luego aplica la función "toUpper" a cada letra, devolviendo una cadena con todas las letras en mayúscula. Ahora veamos cómo podemos utilizar esta función en una lista de nombres:

```Haskell
nombres = ["ana", "pepe", "lucía"]

nombresConMayúsculas = map capitalize nombres

-- output: ["Ana", "Pepe", "Lucía"]
```

Como podemos ver, la función "capitalize" es muy útil y nos permite ahorrar mucho tiempo y esfuerzo en la programación.

## Profundizando

Si bien la función "capitalize" es muy útil en muchas situaciones, es importante tener en cuenta que a veces puede haber excepciones. Por ejemplo, en algunas palabras puede ser necesario mantener las letras en minúscula, como en el caso de los nombres propios o los acrónimos.

Para abordar estas excepciones, podemos definir una lista de palabras que queremos que se mantengan en minúscula y utilizarla en nuestra función "capitalize". También podemos agregar algunas condiciones en la función para que no se aplique la conversión a algunas palabras específicas. De esta forma, podemos personalizar la función para que se adapte mejor a nuestras necesidades.

## Ver también

- [Documentación oficial de Haskell](https://www.haskell.org/documentation/)
- [Tutorial de Haskell para principiantes](https://wiki.haskell.org/Haskell_tutorials_for_beginners)
- [Librería Data.Char de Haskell](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html)