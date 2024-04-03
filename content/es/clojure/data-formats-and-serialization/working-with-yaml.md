---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:55.185611-07:00
description: "YAML, un acr\xF3nimo recursivo de \"YAML Ain't Markup Language\" (YAML\
  \ no es un lenguaje de marcado), es un formato de serializaci\xF3n de datos legible\
  \ por\u2026"
lastmod: '2024-03-13T22:44:58.678852-06:00'
model: gpt-4-0125-preview
summary: "YAML, un acr\xF3nimo recursivo de \"YAML Ain't Markup Language\" (YAML no\
  \ es un lenguaje de marcado), es un formato de serializaci\xF3n de datos legible\
  \ por humanos utilizado para archivos de configuraci\xF3n e intercambio de datos\
  \ entre lenguajes con estructuras de datos diferentes."
title: Trabajando con YAML
weight: 41
---

## Cómo hacerlo:
Clojure no incluye soporte incorporado para YAML, pero puedes utilizar bibliotecas de terceros como `clj-yaml` para analizar y generar datos en YAML. Primero, adiciona la biblioteca a las dependencias de tu proyecto:

```clojure
;; Agrega esto a las dependencias de tu project.clj
[clj-yaml "0.7.0"]
```

Aquí te mostramos cómo puedes usar `clj-yaml` para analizar YAML y convertir mapas de Clojure en YAML.

### Analizando YAML:
```clojure
(require '[clj-yaml.core :as yaml])

;; Analizando una cadena YAML
(let [yaml-str "name: John Doe\nage: 30\nlanguages:\n  - Clojure\n  - Python"]
  (yaml/parse-string yaml-str))
;; Salida:
;; => {"name" "John Doe", "age" 30, "languages" ["Clojure" "Python"]}
```

### Generando YAML desde Clojure:
```clojure
(require '[clj-yaml.core :as yaml])

;; Convirtiendo un mapa de Clojure en una cadena YAML
(let [data-map {:name "Jane Doe" :age 28 :languages ["Java" "Ruby"]}]
  (yaml/generate-string data-map))
;; Salida:
; "age: 28\nlanguages:\n- Java\n- Ruby\nname: Jane Doe\n"
```

Estas operaciones simples con `clj-yaml` se pueden integrar en aplicaciones Clojure para manejar archivos de configuración o facilitar el intercambio de datos con otros servicios o componentes que usen YAML.
