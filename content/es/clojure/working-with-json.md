---
title:                "Trabajando con JSON"
date:                  2024-01-19
simple_title:         "Trabajando con JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Trabajar con JSON (JavaScript Object Notation) es manejar un formato ligero de intercambio de datos. Los programadores lo usan por su facilidad para ser leído por humanos y máquinas, así como su amplia integración en APIs y servicios web.

## How to:
Clojure ofrece varias librerías para trabajar con JSON. `Cheshire` es una opción popular. Primero, añade la dependencia a tu proyecto:

```clojure
[cheshire "5.10.1"]
```

Para parsear un string JSON a un map de Clojure:

```clojure
(require '[cheshire.core :as json])

(json/parse-string "{\"name\":\"John\", \"age\":30}")
;; => {"name" "John", "age" 30}
```

Para convertir un map de Clojure a un string JSON:

```clojure
(json/generate-string {"name" "John", "age" 30})
;; => "{\"name\":\"John\",\"age\":30}"
```

## Deep Dive
JSON nació en los 2000, facilitando la interacción entre servidores y aplicaciones web. Alternativas incluyen XML y YAML, cada uno con sus propias ventajas. En Clojure, trabajar con JSON implica convertir entre estructuras de Clojure y la representación de JSON, manteniendo la simplicidad sintáctica del lenguaje.

## See Also
- Documentación de Cheshire: [Cheshire GitHub](https://github.com/dakrone/cheshire)
- Especificación oficial de JSON: [JSON.org](http://json.org/)
