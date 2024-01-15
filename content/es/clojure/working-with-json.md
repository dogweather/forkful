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

## ¿Por qué trabajar con JSON en Clojure?

Si estás buscando una forma sencilla y eficiente de manejar datos en tu aplicación Clojure, JSON es una excelente opción. Al ser un formato de datos ampliamente utilizado, trabajar con JSON te permitirá intercambiar información con otras aplicaciones y servicios de manera fluida.

## Cómo hacerlo

Para trabajar con JSON en Clojure, primero necesitas importar la biblioteca `clojure.data.json`. Luego, puedes usar la función `json/read-str` para convertir una cadena JSON en una estructura de datos Clojure. Por ejemplo:

```Clojure
(require '[clojure.data.json :as json])
(def json-data "{\"nombre\": \"Juan\", \"edad\": 30}")
(def datos (json/read-str json-data))
```

En este ejemplo, estamos convirtiendo una cadena JSON con un objeto que contiene un nombre y una edad en un mapa de Clojure. Puedes acceder a los valores del mapa usando las claves correspondientes:

```Clojure
(def nombre (get datos "nombre"))
(def edad (get datos "edad"))
```

También puedes usar la función `json/str` para convertir una estructura de datos Clojure en una cadena JSON:

```Clojure
(def datos-2 {"nombre": "María", "edad": 28})
(println (json/str datos-2))
;; La salida será: {"nombre": "María", "edad": 28}
```

## Profundizando

Además de las funciones básicas `json/read-str` y `json/str`, la biblioteca `clojure.data.json` ofrece una amplia gama de opciones para trabajar con JSON en Clojure. Por ejemplo, puedes usar las funciones `json/read` y `json/write` para leer y escribir archivos JSON, o la función `json/parse-string` para convertir cadenas JSON con contenido válido en estructuras de datos Clojure.

También puedes manipular y transformar datos JSON utilizando las funciones `json/select-keys` y `json/merge`, entre otras.

## Ver también

- [Documentación de la biblioteca clojure.data.json](https://github.com/clojure/data.json)
- [Tutorial de Clojure para principiantes](https://clojure.org/guides/getting_started)
- [Introducción a JSON en Clojure](https://www.programming-books.io/essential/clojure-json-c8ea2f62cbce439cb443665469c78394)