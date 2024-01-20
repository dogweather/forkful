---
title:                "Análisis sintáctico de html"
html_title:           "Ruby: Análisis sintáctico de html"
simple_title:         "Análisis sintáctico de html"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/parsing-html.md"
---

{{< edit_this_page >}}

# Parseo HTML en Clojure para principiantes

## ¿Qué & Por qué?

El parseo HTML implica el proceso de decodificar y entender la estructura de una página web. Como programadores, lo hacemos para extraer información útil y manipular, analizar o presentar los datos de la web de una manera más manejable.

## Cómo hacerlo:

Usaremos la biblioteca `clj-tagsoup` para parsear HTML en Clojure. Sigue estos pasos:

### Paso 1: Añadir clj-tagsoup a tu archivo project.clj
```Clojure
:dependencies [[clj-tagsoup "0.3.0"]]
```

### Paso 2: Importar clj-tagsoup
```Clojure
(ns my.ns
  (:require [clj-tagsoup.core :as soup]))
```

### Paso 3: Parsear HTML
```Clojure
(def page (soup/parse "https://www.paginaweb.com/"))
```

Aquí podría ser el resultado:
```Clojure
{:tag :html, :attrs {}, :content [{:tag :head...}{:tag :body...}]}
```

## Inmersión profunda

Históricamente, el parseo de HTML ha sido problemático debido a la flexibilidad y tolerancia de errores de HTML. Afortunadamente, `clj-tagsoup` facilita este proceso.

A pesar de que `clj-tagsoup` es una herramienta fantástica, hay varios métodos alternativos para parsear HTML en Clojure. Por ejemplo, `Enlive` y `Hickory` son también soluciones viables, pero pueden tener una curva de aprendizaje más empinada.

`clj-tagsoup` opera tomando HTML mal formado y generando un árbol de sintaxis abstracta (AST). Una vez que tenemos este AST, podemos recorrerlo y buscar los datos que nos interesan.

## Ver también

- Documentación de clj-tagsoup: https://github.com/nathell/clj-tagsoup
- HTML simple parseo con Enlive: https://github.com/cgrand/enlive
- Introducción a Hickory: https://github.com/davidsantiago/hickory