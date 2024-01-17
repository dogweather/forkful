---
title:                "Mayusculizar una cadena"
html_title:           "Clojure: Mayusculizar una cadena"
simple_title:         "Mayusculizar una cadena"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Capitalizar una cadena de texto significa escribir la primera letra de cada palabra en mayúscula. Los programadores suelen hacerlo para mejorar la legibilidad del código y seguir convenciones de estilo establecidas.

## Cómo hacerlo:
```Clojure
(require '[clojure.string :as str])
(str/capitalize "hola mundo") ;=> "Hola mundo"
(str/capitalize "hola mundo feliz") ;=> "Hola Mundo Feliz"
```

## Profundizando:
Capitalizar palabras en un lenguaje de programación es una práctica común que proviene de las convenciones de escritura en inglés, donde se capitalizan las primeras letras de los nombres de funciones y variables. Alternativas a la función `capitalize` en Clojure incluyen `upper-case` y `title-case`, que tienen diferentes reglas de capitalización. La implementación de la función `capitalize` se basa en la librería Apache Commons Lang, que utiliza algoritmos para identificar la primera letra de cada palabra y convertirla a mayúscula.

## Ver también:
- Documentación oficial de Clojure sobre la función `capitalize`: https://clojuredocs.org/clojure.core/capitalize
- Ejemplos de convenciones de estilo para escribir código en Clojure: https://github.com/bbatsov/clojure-style-guide