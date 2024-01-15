---
title:                "Analizando HTML"
html_title:           "Haskell: Analizando HTML"
simple_title:         "Analizando HTML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has querido extraer información de una página web y utilizarla en tu programa? La respuesta es simple: ¡necesitas aprender a analizar HTML en Haskell!

## Cómo hacerlo
La principal herramienta que utilizaremos en Haskell para analizar HTML es la biblioteca `tagsoup`. Primero, necesitaremos importarla en nuestro código:

```Haskell
import Text.HTML.TagSoup -- importamos la biblioteca
```

A continuación, debemos cargar el HTML que queremos analizar. Esto se puede hacer de varias maneras, pero una forma común es utilizando la función `openUrl` de la biblioteca `HTTP`:

```Haskell
import Network.HTTP

url <- simpleHTTP (getRequest "https://www.ejemplo.com") >>= getResponseBody
```

Esto nos dará una cadena de texto con el código HTML de esa página en particular. Ahora, podemos utilizar la función `parseTags` para convertir esa cadena de texto en una lista de etiquetas que podemos recorrer y analizar:

```Haskell
let tags = parseTags url -- convertimos la cadena de texto a una lista de etiquetas
```

Y ahora, por ejemplo, si queremos extraer todos los enlaces de la página, podemos utilizar la función `isTagOpenName` para filtrar solo las etiquetas de apertura con el nombre "a" (enlaces) y luego utilizar la función `fromAttrib` para obtener el valor del atributo "href" de cada etiqueta:
```Haskell
let links = map (fromAttrib "href") (filter (isTagOpenName "a") tags) -- lista con los enlaces de la página
```

Esto es solo un ejemplo sencillo, pero con la biblioteca `tagsoup` y algunas otras herramientas de Haskell, podemos analizar y extraer cualquier información que necesitemos de una página web.

## Inmersión profunda
Si quieres profundizar más en el análisis de HTML en Haskell, puedes echar un vistazo a la documentación oficial de la biblioteca `tagsoup` y también a otras herramientas como `HXT` que proporcionan un enfoque más modular y flexible para el análisis de HTML.

## Ver también
- [Documentación de la biblioteca `tagsoup`](https://hackage.haskell.org/package/tagsoup)
- [Documentación de la biblioteca `HXT`](https://hackage.haskell.org/package/HXT)