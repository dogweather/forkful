---
title:                "Trabajando con YAML"
html_title:           "Arduino: Trabajando con YAML"
simple_title:         "Trabajando con YAML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Trabajamos con YAML porque es fácil de leer y escribir para los humanos. Se usa en configuración de aplicaciones, archivos de Docker, y Kubernetes, entre otros.

## How to:
Para leer y escribir YAML en Clojure, usaremos la biblioteca `clj-yaml`. Primero, instálala con [Leiningen](https://leiningen.org/) o [Clojure CLI tools](https://clojure.org/guides/deps_and_cli).

```Clojure
;; Añade clj-yaml a tu proyecto.clj o deps.edn
[clj-yaml "0.7.0"] ; agregar la versión más reciente

;; Usa clj-yaml en tu código
(require '[clj-yaml.core :as yaml])

;; Cargar YAML de un string
(def yaml-string "
frutas:
  - manzana
  - banana
  - uva")

(def datos (yaml/parse-string yaml-string))
;; datos es un mapa de Clojure: {:frutas ["manzana" "banana" "uva"]}

;; Convertir mapa de Clojure a YAML
(def clojure-map {:hello "mundo" :number 42})
(def yaml-data (yaml/generate-string clojure-map))
;; yaml-data es un string YAML: "hello: mundo\nnumber: 42\n"
```

## Deep Dive:
YAML, que significa "YAML Ain't Markup Language", nació en 2001 como alternativa al XML. Es común en el ecosistema de DevOps. Alternativas incluyen JSON y TOML, pero YAML es preferido cuando la legibilidad es crucial. En Clojure, trabajar con YAML es sencillo gracias a bibliotecas como `clj-yaml` que se apoyan en la librería Java SnakeYAML para el procesamiento.

## See Also:
- Documentación de `clj-yaml`: https://github.com/clj-commons/clj-yaml
- YAML oficial: https://yaml.org
- Guía de referencia rápida de YAML: https://learnxinyminutes.com/docs/yaml/
