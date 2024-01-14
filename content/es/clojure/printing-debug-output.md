---
title:    "Clojure: Impresión de salida de depuración"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has encontrado en una situación en la que tu código no funciona como debería y no tienes ni idea de por qué? Esa es la razón por la que imprimir salida de depuración (debug output) puede ser útil. Al imprimir el valor de variables y hacer un seguimiento del flujo de tu programa, podrás identificar fácilmente dónde está el error y cómo solucionarlo.

## Cómo hacerlo

Utilizar la función `println` es la forma más básica de imprimir debug output en Clojure. Por ejemplo:

```Clojure
(defn suma [a b]
  (println "El valor de a es" a)
  (println "El valor de b es" b)
  (+ a b))

(suma 2 3)
```

La salida sería:

```
El valor de a es 2
El valor de b es 3
5
```

También puedes utilizar `prn` para imprimir valores sin ninguna información adicional, o `pr-str` para obtener una representación en forma de string de un valor. Ambas funciones aceptan múltiples argumentos y los imprimen en una sola línea.

## Profundizando

Además de las funciones mencionadas anteriormente, también puedes utilizar la macro `dbg` del librería tools.cli, que te permite especificar qué valores quieres imprimir en tu código. Por ejemplo:

```Clojure
(ns example.core
  (:require [clojure.tools.cli :refer [dbg]]))

(defn suma [a b]
  (let [a (dbg a)
        b (dbg b)]
    (+ a b)))

(suma 2 3)
```

La salida sería:

```
a=2  b=3
5
```

También puedes utilizar un deshabilitador de debug output en producción para evitar que se impriman valores innecesarios y así mejorar el rendimiento de tu programa.

## Ver también

- [Documentación oficial de Clojure sobre imprimir valores](https://clojuredocs.org/clojure.core/println)
- [Artículo en CódigoFacilito sobre la depuración de programas en Clojure](https://codigofacilito.com/articulos/debug-clojure)