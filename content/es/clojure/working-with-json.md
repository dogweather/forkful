---
title:                "Trabajando con json"
html_title:           "Clojure: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Trabajar con JSON es una forma de manejar información estructurada en formato de texto. Los programadores lo hacen para facilitar el intercambio de datos entre diferentes sistemas y lenguajes.

## Cómo:
```Clojure
;; Crear un objeto JSON
(def objeto {:nombre "Juan" :edad 25 :trabajo "programador"})

;; Convertir un objeto a formato JSON
(require '[cheshire.core :refer [generate-string]])
(generate-string objeto)
;; {"nombre": "Juan", "edad": 25, "trabajo": "programador"}

;; Leer un archivo JSON
(require '[clojure.data.json :as json])
(json/read-str "{\"nombre\": \"Juan\", \"edad\": 25, \"trabajo\": \"programador\"}")
;; {:nombre "Juan", :edad 25, :trabajo "programador"}
```

## Deep Dive:
JSON (JavaScript Object Notation) es un formato de texto basado en JavaScript para almacenar y transmitir datos estructurados. Fue creado en 2001 y se ha vuelto popular debido a su simplicidad y facilidad de uso. Algunas alternativas a JSON incluyen XML y YAML, pero JSON sigue siendo el preferido por su legibilidad y facilidad de integración con JavaScript y otros lenguajes. En Clojure, existen diversas librerías para trabajar con JSON, siendo cheshire una de las más populares.

## Ver también:
- [Documentación oficial de JSON](https://www.json.org/json-es.html)
- [Librería cheshire para trabajar con JSON en Clojure](https://github.com/dakrone/cheshire)