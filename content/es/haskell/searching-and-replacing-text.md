---
title:    "Haskell: Buscar y reemplazar texto"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por qué

Reemplazar texto es una tarea común en la programación, ya sea para corregir errores o para actualizar información. Al utilizar Haskell, puedes hacerlo de manera eficiente y sencilla. En este artículo, aprenderás cómo realizar búsquedas y reemplazos de texto en Haskell.

## Cómo

Primero, importaremos el módulo `Text.Regex.TDFA` para utilizar sus funciones de búsqueda y reemplazo de texto. Luego, usaremos la función `subRegex` para realizar el reemplazo en una cadena de texto. Aquí hay un ejemplo de cómo reemplazar todas las apariciones de la palabra "mundo" por "Haskell":

```Haskell
import Text.Regex.TDFA

texto = "Hola mundo, bienvenido a mi blog sobre Haskell."

nuevoTexto = subRegex (mkRegex "mundo") texto "Haskell"

print nuevoTexto -- salida: "Hola Haskell, bienvenido a mi blog sobre Haskell."
```

Podemos ver que la función `subRegex` recibe tres parámetros: una expresión regular, la cadena de texto original y la cadena de texto que queremos reemplazar. Además, utilizamos la función `mkRegex` para crear una expresión regular a partir de un patrón dado.

También podemos utilizar la función `subRegexAll` para reemplazar todas las apariciones de la palabra "mundo" por "Haskell" y contar cuántos reemplazos se realizaron:

```Haskell
nuevoTexto = subRegexAll (mkRegex "mundo") texto "Haskell"

print nuevoTexto -- salida: (2, "Hola Haskell, bienvenido a mi blog sobre Haskell.")
-- 2 representa la cantidad de reemplazos realizados
```

Además de la función `subRegex`, el módulo `Text.Regex.TDFA` también cuenta con otras funciones útiles para realizar búsquedas y reemplazos, como `matchOnceText` para encontrar la primera coincidencia de una expresión regular en una cadena de texto.

## Profundizando

En Haskell, las expresiones regulares se representan como valores del tipo `Regex`, que se pueden crear utilizando la función `mkRegex`, como vimos anteriormente. Además, el módulo `Text.Regex.TDFA` ofrece un tipo de datos adicional llamado `RegexOptions`, que le permite al usuario especificar opciones personalizadas para su expresión regular, como el modo insensible a mayúsculas y minúsculas.

También es importante destacar que el reemplazo de texto en Haskell es inmutable, lo que significa que las cadenas de texto originales no se modifican, sino que se crean nuevas cadenas con el reemplazo realizado. Esto es una característica importante de Haskell ya que evita efectos secundarios inesperados.

## Ver también

- [Documentación oficial de Haskell sobre expresiones regulares](https://www.haskell.org/hoogle/?hoogle=Text.Regex.TDFA)
- [Expresiones regulares en el libro de Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell/Understanding_arrows)
- [Tutorial de expresiones regulares en FOSDEM](https://fosdem.org/2014/schedule/event/regexintro/)