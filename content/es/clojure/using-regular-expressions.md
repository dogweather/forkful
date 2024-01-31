---
title:                "Uso de expresiones regulares"
date:                  2024-01-19
simple_title:         "Uso de expresiones regulares"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Las expresiones regulares son patrones usados para encontrar coincidencias en textos, como palabras o secuencias específicas. Los programadores las utilizan para validar, buscar, editar, y manipular datos de manera eficiente y concisa.

## Cómo Hacerlo:
```Clojure
;; Comprobando si un string contiene un número
(re-find #"\d+" "La fecha es 13/04/2023")
;; => "13"

;; Extrayendo todos los números de un string
(re-seq #"\d+" "Pedido Nº 234: 10 items")
;; => ("234" "10")

;; Reemplazando coincidencias
(clojure.string/replace "23/04/2023" #"/" "-")
;; => "23-04-2023"

;; Validando un formato de email simple
(defn validar-email [email]
  (re-matches #".+@.+\..+" email))

(validar-email "usuario@example.com")
;; => "usuario@example.com"

(validar-email "no es email")
;; => nil
```

## Inmersión Profunda
Las expresiones regulares se originaron en la década de 1950 con el trabajo teórico de Stephen Kleene. Alternativas a las expresiones regulares incluyen el uso de parsers o bibliotecas de manipulación de texto específicas. En Clojure, trabajamos con regex haciendo uso de Java bajo el capó, por lo que su rendimiento es sólido.

## Ver También
- [ClojureDocs](https://clojuredocs.org/), para ejemplos y documentación extra de funciones Clojure.
- [The Java Tutorials - Regex](https://docs.oracle.com/javase/tutorial/essential/regex/), para referencia detallada en la implementación Java de expresiones regulares.
