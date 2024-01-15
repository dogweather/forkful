---
title:                "Analizando HTML."
html_title:           "Clojure: Analizando HTML."
simple_title:         "Analizando HTML."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Por qué

Puede que te encuentres en una situación en la que necesitas extraer información específica de una página web en formato HTML. Tal vez estás construyendo una herramienta de scrapping o una aplicación para recopilar datos, y necesitas una forma eficiente de parsear el HTML para obtener solo los datos que necesitas. Esta es exactamente la razón por la que deberías conocer cómo parsear HTML en Clojure.

## Cómo hacerlo

Afortunadamente, Clojure tiene una librería incorporada llamada `clojure.data.xml`, que te permite parsear fácilmente HTML. Primero, necesitas importar la librería en tu código:

```
(ns my-html-parser.core
  (:require [clojure.data.xml :as xml]))
```

Luego, puedes usar la función `clojure.data.xml/parse` para obtener un árbol de datos del HTML:

```
(def html-tree (xml/parse "<p>¡Hola mundo!</p>"))
```

Ahora, podemos usar funciones de navegación de datos para extraer información específica del árbol de datos. Por ejemplo, si queremos obtener el contenido de la etiqueta `p` en el HTML, podemos hacer lo siguiente:

```
(xml/text (some-> html-tree
                    :content
                    first))
;; Salida: ¡Hola mundo!
```

Puedes ver que la función `xml/text` toma una estructura de datos (en este caso, la primera etiqueta `p`) y devuelve su contenido como una cadena de texto.

## Profundizando

Ahora que ya sabes cómo parsear HTML en Clojure, puede que te preguntes cómo funciona realmente la librería `clojure.data.xml`. Básicamente, esta librería transforma el HTML en un árbol de datos basado en el estándar XML. Esto significa que puedes usar funciones de navegación de datos como `first`, `nth` y `get` para acceder a elementos específicos dentro del árbol de datos.

También es importante señalar que `clojure.data.xml` soporta tanto HTML como XML, por lo que puedes usarla para parsear ambos tipos de documentos.

## Véase también

- Documentación oficial de Clojure sobre la librería `clojure.data.xml`: https://clojure.org/guides/learn/parsing_xml
- Tutorial práctico sobre cómo parsear HTML en Clojure: https://purelyfunctional.tv/guide/how-to-parse-html-in-clojure/
- Guía de aprendizaje sobre Clojure: https://clojure.org/about/functional_programming