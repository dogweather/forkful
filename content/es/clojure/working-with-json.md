---
title:                "Clojure: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con JSON en Clojure?

Trabajar con JSON en Clojure es una tarea común en muchas aplicaciones web y de backend. JSON, o JavaScript Object Notation, es un formato de intercambio de datos ligero y fácil de leer que se utiliza para transmitir información entre diferentes sistemas. Usar Clojure para trabajar con JSON es una excelente opción debido a su capacidad para manejar estructuras de datos complejas y su sintaxis simple y funcional.

## Cómo trabajar con JSON en Clojure

Para trabajar con JSON en Clojure, primero debemos tener una comprensión básica de cómo se estructuran los datos en formato JSON. Un objeto JSON consiste en pares de clave-valor, con la clave siendo una cadena y el valor puede ser cualquier tipo de dato válido en JSON (contenedor de objetos, matriz, número, cadena, valor booleano, etc.). En Clojure, podemos representar un objeto JSON como una estructura de datos de mapas utilizando la función `json/read-str` de la biblioteca clojure.data.json.

```Clojure
(ns ejemplo-json.core
  (:require [clojure.data.json :as json]))

(def obj-json (json/read-str "{\"nombre\": \"Juan\", \"edad\": 30 }"))
(print obj-json)

;; Output:
{ :nombre "Juan"
  :edad 30 }
```

Para acceder a los valores de un objeto JSON, podemos usar la función `get` o la sintaxis de acceso a claves de un mapa. En este ejemplo, accederemos al nombre y la edad de nuestro objeto JSON.

```Clojure
(get obj-json :nombre)
;; Output: "Juan"

(:edad obj-json)
;; Output: 30
```

Para trabajar con matrices JSON, podemos usar la función `json/read-str` de la misma manera. En el siguiente ejemplo, nuestra matriz JSON contiene dos objetos que representan personas. Para acceder a la información de cada persona, usaremos la función `each` de la biblioteca `clojure.walk`.

```Clojure
(def arr-json (json/read-str "[{\"nombre\": \"Maria\", \"edad\": 25 }, {\"nombre\" : \"Pedro\", \"edad\": 35 }]"))
(clojure.walk/each print arr-json)

;; Output:
{ :nombre "Maria"
  :edad 25 }
{ :nombre "Pedro"
  :edad 35 }
```

## Deep Dive: Trabajando con JSON en Clojure

Cuando trabajamos con JSON en Clojure, es importante asegurarnos de que los datos están correctamente estructurados antes de intentar acceder a ellos. Podemos utilizar la función `json/validate` para validar la sintaxis de un objeto JSON y asegurarnos de que cumple con nuestras expectativas de datos.

También es importante tener en cuenta que Clojure es un lenguaje funcional y, por lo tanto, trabajaremos más eficientemente con JSON utilizando funciones de orden superior como `map` y `filter` en lugar de estructuras de control imperativas.

## Ver También

- [Documentación de la biblioteca `clojure.data.json`](https://github.com/clojure/data.json)
- [Guía de referencia rápida de JSON en Clojure](https://gist.github.com/jenkstom/4244547)
- [Tutorial de Clojure con ejemplos de JSON](https://practicalli.github.io/clojure/working-with-json/)