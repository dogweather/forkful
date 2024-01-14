---
title:                "Clojure: Analizando html"
simple_title:         "Analizando html"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un programador que trabaja en aplicaciones web, es posible que en algún momento necesites acceder a información contenida en una página HTML. Aquí es donde entra en juego el análisis o "parsing" de HTML. Esta técnica te permite extraer datos específicos de una página web y utilizarlos en tu código.

## Cómo hacerlo

En Clojure, podemos utilizar la biblioteca "enlive" para realizar el análisis de HTML de manera sencilla. Primero, necesitamos importar la biblioteca en nuestro código:

```Clojure
(ns mi-aplicacion
  (:require [net.cgrand.enlive-html :refer [html->]]))
```

Luego, podemos utilizar la función `html->` para indicar qué parte del HTML queremos analizar. Por ejemplo, si queremos obtener todos los enlaces de una página, podríamos usar el siguiente código:

```Clojure
(html-> "<a href='https://mi-pagina-web.com'>Enlace</a>" [:a :href])
```

Esto nos devolverá un vector con todos los enlaces encontrados en el HTML: `["https://mi-pagina-web.com"]`. Podemos utilizar esta misma técnica para obtener otros elementos como textos, imágenes o tablas.

## Profundizando

Dependiendo de la complejidad del HTML que estamos analizando, puede ser necesario realizar operaciones adicionales para obtener los datos deseados. Por ejemplo, si queremos obtener el texto dentro de un elemento `<div>` con una clase específica, podemos utilizar la función `text` para obtener solo el texto y no el HTML completo.

```Clojure
(html-> "<div class='texto'><p>Bienvenidos a mi blog</p></div>" [:div.texto] text)
```

Esto nos devolverá `Bienvenidos a mi blog`. También podemos realizar filtros y manipulaciones en los resultados utilizando funciones como `filter` y `map`.

## Ver también

- [Documentación de enlive](https://github.com/cgrand/enlive)
- [Ejemplos de enlive](https://github.com/cgrand/enlive/wiki/Examples)
- [Manual de Clojure](https://clojure.org/guides/learn/syntax) (en español)