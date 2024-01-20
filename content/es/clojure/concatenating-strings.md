---
title:                "Concatenando cadenas de texto"
html_title:           "Arduino: Concatenando cadenas de texto"
simple_title:         "Concatenando cadenas de texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué es y Por Qué?

La concatenación de cadenas es unir dos o más cadenas para crear una nueva cadena. Los programadores la usan frecuentemente para combinar y manipular datos en un formato legible.

## Cómo Funciona:

Clojure, un lenguaje funcional dinámico, no incluye una función innata de concatenación de cadenas. En su lugar, utiliza funciones como `str` y `join`.

```Clojure
;; Ejemplo de concatenación con `str`
(defn saludo
  [nombre]
  (str "Hola, " nombre))

(saludo "Juan")
;; Salida: "Hola, Juan
```

```Clojure
;; Ejemplo de concatenación con `join` 
(defn lista-numeros
  [numeros]
  (clojure.string/join ", " numeros))

(lista-numeros ["uno" "dos" "tres"])
;; Salida: "uno, dos, tres"
```

## Un Vistazo Más Profundo

La falta de una función innata para la concatenación de cadenas en Clojure se remonta a los fundamentos funcionales de este lenguaje. A diferencia de los lenguajes imperativos que pueden depender de la manipulación en su lugar, Clojure prioriza la inmutabilidad. por lo cual, en lugar de cambiar una cadena existente, crea una nueva.

Existen una serie de alternativas a `str` y `join`, cada una con sus propias ventajas y desventajas. `format`, por ejemplo, puede ser útil para concatenar y dar formato simultáneamente:

```Clojure
(format "Hola %s, tienes %d años." "Juan" 30)
;; Salida: "Hola Juan, tienes 30 años."
```

Al decidir qué método utilizar, considere la legibilidad y claridad del código, así como también las implicaciones de rendimiento. Clojure es una capa construida sobre Java, por lo que las operaciones de concatenación de cadenas pueden generar nuevas instancias de las cadenas, lo que consume memoria y tiempo.

## Ver También

Para más información, consulta las siguientes fuentes:

- Documentación oficial de Clojure: https://clojure.org/
- Guía de Clojure para principiantes: https://www.braveclojure.com/
- Documentación oficial de las funciones Strings en Clojure: https://clojuredocs.org/clojure.string