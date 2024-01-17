---
title:                "Extrayendo subcadenas"
html_title:           "Clojure: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Extraer subcadenas es una técnica común en programación para obtener una parte específica de una cadena de caracteres. Esto puede ser útil para manipular datos, validar entradas de usuario o generar salidas formateadas. Los programadores suelen usar esta técnica para obtener una mayor flexibilidad en el manejo de cadenas de texto.

## Cómo:
En Clojure, puedes utilizar la función `subs` para extraer una subcadena de una cadena existente. Por ejemplo:

```Clojure
(subs "Hola mundo" 0 4)
```

Este código devolverá la subcadena "Hola" de la cadena original. Los dos números después de la cadena indican el índice de inicio y el índice de final respectivamente.
 
También puedes utilizar `subs` con cadenas almacenadas en variables:

```Clojure
(def mensaje "Hola mundo")

(subs mensaje 5 10)
```

La salida para este ejemplo sería "mundo".

## Profundizando:
Extraer subcadenas ha sido una técnica utilizada en programación durante mucho tiempo. En lenguajes más antiguos como C, se utilizaba una función llamada `substr` con una sintaxis similar a la de `subs` en Clojure. Sin embargo, en lenguajes más modernos como Python, esta funcionalidad se integra directamente en el tipo de dato de cadena.

En Clojure, también puedes utilizar patrones de expresiones regulares para extraer subcadenas. Esto puede ser útil si necesitas obtener una parte de una cadena que cumpla ciertas condiciones.

Si bien `subs` es la forma más sencilla de extraer subcadenas en Clojure, también existen otras funciones como `substring` y `split-at` que pueden ser útiles en otras situaciones. Puedes aprender más sobre estas funciones en la documentación de Clojure.

## Ver también:
- La documentación de Clojure sobre la función `subs`: https://clojuredocs.org/clojure.core/subs
- La documentación de Clojure sobre expresiones regulares: https://clojuredocs.org/clojure.core/re-matches