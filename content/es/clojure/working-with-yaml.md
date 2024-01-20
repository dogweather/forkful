---
title:                "Trabajando con yaml"
html_title:           "Clojure: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Trabajar con YAML es una forma simple y estructurada de almacenar y transmitir datos. Los programadores utilizan YAML porque permite una fácil lectura y escritura, y es compatible con una amplia gama de lenguajes de programación.

## Cómo hacerlo:
```Clojure
;; Importar la librería YAML
(require '[yaml.core :as yaml])

;; Crear un mapa con datos
(def datos {:nombre "Juan" :edad 25 :lenguajes ["Clojure" "Python" "Java"]})

;; Convertir el mapa a formato YAML
(yaml/generate-string datos)

;; Salida: "nombre: Juan\nedad: 25\nlenguajes:\n - Clojure\n - Python\n - Java"
```

## Profundizando:
YAML fue creado por Clark Evans en el 2001 para solucionar problemas comunes en la configuración de proyectos. Algunas alternativas a YAML son JSON y XML, pero YAML es más legible y fácil de escribir. En Clojure, YAML se implementa a través de la librería "yaml-clojure".

## Ver también:
- [Documentación oficial de YAML](https://yaml.org/)