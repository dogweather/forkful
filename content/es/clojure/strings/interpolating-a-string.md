---
date: 2024-01-20 17:50:51.737620-07:00
description: "C\xF3mo Hacerlo: En Clojure, no hay una funci\xF3n directa para la interpolaci\xF3\
  n de cadenas como en otros lenguajes, pero se puede lograr con `str` y `format`.\u2026"
lastmod: '2024-03-13T22:44:58.642786-06:00'
model: gpt-4-1106-preview
summary: "En Clojure, no hay una funci\xF3n directa para la interpolaci\xF3n de cadenas\
  \ como en otros lenguajes, pero se puede lograr con `str` y `format`."
title: "Interpolaci\xF3n de cadenas de texto"
weight: 8
---

## Cómo Hacerlo:
En Clojure, no hay una función directa para la interpolación de cadenas como en otros lenguajes, pero se puede lograr con `str` y `format`. Aquí unos ejemplos:

```Clojure
;; Usando str para concatenar
(def nombre "Mundo")
(println (str "Hola, " nombre "!"))  ; Muestra "Hola, Mundo!"

;; Usando format para interpolación estilo printf
(def edad 30)
(println (format "Tengo %d años." edad))  ; Muestra "Tengo 30 años."
```

Con clojure.string también puedes hacer algo similar:

```Clojure
(require '[clojure.string :as s])

;; Concatenando con join
(println (s/join ["Hola, " nombre "!"]))  ; Muestra "Hola, Mundo!"
```

## Profundizando
La interpolación de cadenas es un concepto antiguo, común en muchos lenguajes como Python y JavaScript. En Clojure, siendo un lenguaje funcional, prioriza la inmutabilidad y la simplicidad, por eso no tiene una sintaxis especial para interpolación. 

Sin embargo, puedes implementar una función de interpolación o usar librerías como `clojure.string`. Otra opción es usar el paquete `selmer`, que ofrece una sintaxis similar a Django para la interpolación de cadenas.

Aquí un ejemplo con `selmer`:

```Clojure
(require '[selmer.parser :as parser])

(def template "Hola, {{ name }}!")
(println (parser/render template {:name "Mundo"}))  ; Muestra "Hola, Mundo!"
```

Además, es importante comprender que en Clojure, la concatenación de strings con `str` es bastante eficiente gracias a la forma en que la JVM maneja los strings.

## Ver También
- Clojure Docs str: [https://clojuredocs.org/clojure.core/str](https://clojuredocs.org/clojure.core/str)
- Selmer templating library: [https://github.com/yogthos/Selmer](https://github.com/yogthos/Selmer)
- Java's String.format: [https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#format(java.lang.String,%20java.lang.Object...)](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#format(java.lang.String,%20java.lang.Object...))
