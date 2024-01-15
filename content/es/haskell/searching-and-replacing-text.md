---
title:                "Buscando y reemplazando texto"
html_title:           "Haskell: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Antes de sumergirnos en cómo buscar y reemplazar texto en Haskell, es importante entender por qué es útil hacerlo. La búsqueda y reemplazo de texto es una tarea común en la programación, ya sea para corregir errores o para hacer cambios en un gran conjunto de datos. Al aprender cómo hacerlo en Haskell, puedes mejorar tu flujo de trabajo y hacer tu código más eficiente.

## Cómo hacerlo

Para buscar y reemplazar texto en Haskell, utilizamos la función `replace` de la librería `Data.Text`. Esta función toma tres argumentos: el texto original, la cadena que quieres buscar y la cadena que quieres reemplazar. Por ejemplo:

```Haskell
import Data.Text

textoOriginal = "Hola mundo"
textoNuevo = replace "Hola" "Adiós" textoOriginal
```

El resultado será `textoNuevo = "Adiós mundo"`, ya que hemos reemplazado la palabra "Hola" por "Adiós". También puedes usar esta función para reemplazar patrones más complejos, como por ejemplo:

```Haskell
import Data.Text

textoOriginal = "Hoy es un día soleado"
textoNuevo = replace "soleado" "lluvioso" textoOriginal
```

En este caso, el resultado será `textoNuevo = "Hoy es un día lluvioso"`. Como puedes ver, la función `replace` es muy versátil y puedes utilizarla para hacer cambios en cualquier tipo de texto.

## Profundizando

Si quieres profundizar más en la búsqueda y reemplazo de texto en Haskell, puedes explorar otras funciones disponibles en la librería `Data.Text`, como `replaceFirst`, que reemplaza solo la primera ocurrencia de una cadena, o `replaceSuffix`, que reemplaza solo la última ocurrencia. También puedes investigar cómo hacer búsquedas y reemplazos usando expresiones regulares con la función `replaceRegex`.

En general, la librería `Data.Text` ofrece muchas herramientas útiles para manipular texto en Haskell. Puedes leer su documentación para descubrir todas las funciones disponibles y explorar diferentes formas de hacer búsquedas y reemplazos.

## Ver también

- Documentación de la librería `Data.Text`: https://hackage.haskell.org/package/text/docs/Data-Text.html
- Tutorial de Haskell de Codecademy: https://www.codecademy.com/es/learn/learn-haskell
- Ejemplos de código de búsquedas y reemplazos en Haskell: https://gist.github.com/tel/1637879