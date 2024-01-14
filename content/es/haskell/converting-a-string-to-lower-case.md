---
title:                "Haskell: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una cadena a minúsculas en Haskell?

Si estás trabajando con manipulación de cadenas en Haskell, es posible que te hayas preguntado cómo convertir una cadena a minúsculas. Esto puede ser útil para estandarizar el formato de las cadenas o para realizar comparaciones de manera más precisa. A continuación, te mostraremos cómo hacerlo de forma sencilla y eficaz.

## Cómo hacerlo

La forma más sencilla de convertir una cadena a minúsculas en Haskell es utilizando la función `toLower` de la biblioteca `Data.Char`. Esta función toma un solo carácter y devuelve su equivalente en minúsculas. Sin embargo, para aplicarla a una cadena completa, es necesario hacer uso de otras funciones y técnicas.

Primero, necesitamos convertir la cadena en una lista de caracteres utilizando la función `words`. Luego, aplicamos la función `toLower` a cada carácter utilizando `map`, y finalmente, juntamos la lista de caracteres nuevamente en una cadena utilizando la función `unwords`. Veamos un ejemplo:

```Haskell
import Data.Char (toLower)

convertirMinusculas :: String -> String
convertirMinusculas cadena = unwords (map toLower (words cadena))
```

Ahora, si aplicamos la función `convertirMinusculas` a una cadena, obtendremos su versión en minúsculas. Por ejemplo, si aplicamos la función a la cadena "Haskell es un lenguaje de programación funcional", obtendremos como resultado "haskell es un lenguaje de programación funcional".

Es importante tener en cuenta que la función `toLower` sólo funciona con caracteres latinos estándar. Si tu cadena contiene caracteres acentuados u otros símbolos, es posible que necesites utilizar una biblioteca externa para una conversión más precisa.

## Profundizando

Si queremos entender mejor cómo funciona la conversión de cadenas a minúsculas en Haskell, podemos dar un vistazo a la implementación de la función `toLower` en la biblioteca `Data.Char`. Esta función utiliza tablas de búsqueda y patrones de coincidencia para determinar el equivalente en minúsculas de un carácter.

Otra forma de convertir una cadena a minúsculas es utilizando la técnica de recursión en Haskell. Esta técnica utiliza patrones de coincidencia para determinar si un carácter es mayúscula, y en ese caso, lo convierte a minúscula y sigue recursivamente con el resto de la cadena. Este enfoque es más eficiente en términos de memoria, pero puede ser más difícil de entender para principiantes.

En resumen, hay varias formas de convertir una cadena a minúsculas en Haskell y cada una tiene sus pros y contras. La mejor opción dependerá del contexto y de los requisitos del código en el que se esté trabajando.

## Ver también

- [Documentación de la biblioteca `Data.Char`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html)
- [Tutorial de Haskell en español](https://wiki.uclm.es/index.php?title=P._36-_%C2%ABHaskell_en_espa%C3%B1ol%C2%BB)
- [Ejemplos prácticos de manipulación de cadenas en Haskell](https://github.com/mulder21c/haskell-cadenas)