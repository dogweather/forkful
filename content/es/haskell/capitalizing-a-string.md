---
title:                "Capitalizando una cadena de texto"
html_title:           "Haskell: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Capitalizar una cadena implica convertir el primer caracter de cada palabra en mayúsculas. Los programadores a menudo lo hacen para mejorar la legibilidad y consistencia de los resultados en la interfaz de usuario.

## Cómo hacerlo:

Puedes capitalizar una cadena en Haskell con la biblioteca `Data.Char`:

```Haskell
import Data.Char (toUpper)

capitaliza :: String -> String
capitaliza [] = []
capitaliza (x:xs) = toUpper x : xs
```

Ejecutar `capitaliza "hola mundo"` te dará `"Hola mundo"`.

Pero si quieres capitalizar cada palabra puedes usar la función `words` y `unwords`:

```Haskell
import Data.Char (toUpper)

capitalizaCadaPalabra :: String -> String
capitalizaCadaPalabra = unwords . map capitaliza . words 
```

Ahora `capitalizaCadaPalabra "hola mundo"` retorna `"Hola Mundo"`.

## Buceo Profundo 

Originariamente, la capitalización se usaba en la antigua Roma, donde todas las letras eran mayúsculas. En la era de las computadoras, es utilizado para mejorar la legibilidad.

Alternativamente, puede usar funciones de librerías de terceros como Text.ICU.Transform (requiere la librería icu). Yet, Haskell's standard `Data.Char` library is sufficient for most use cases.

Haskell implementa la capitalización en un estilo funcional, tomando una cadena y devolviendo una nueva cadena con los cambios. Esta inmutabilidad es parte de por qué Haskell puede ser más predecible que otros lenguajes.

## Ver También 

- Documento de la biblioteca de Haskell [Data.Char](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)