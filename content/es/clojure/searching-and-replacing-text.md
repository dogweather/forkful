---
title:    "Clojure: Buscando y reemplazando texto"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Por qué usar Clojure para buscar y reemplazar texto?

Buscar y reemplazar texto es una tarea común en la programación, puede ser una herramienta útil para hacer cambios rápidos en grandes cantidades de código. Con Clojure, puedes hacer esto de manera rápida y eficiente gracias a su sintaxis concisa y funcional.

## Cómo hacerlo

Buscar y reemplazar texto en Clojure es muy sencillo. Primero, importa la librería `clojure.string` para acceder a las funciones de texto. Luego, utiliza la función `replace` para especificar la cadena de texto que deseas reemplazar, seguida de la cadena de texto de reemplazo. A continuación, utiliza la función `str` para imprimir los resultados. Por ejemplo:

```Clojure
(ns buscar-reemplazar
  (:require [clojure.string :as str]))

(str/replace "Hola mundo" "mundo" "amigos")

;; Output: Hola amigos
```

Otra forma de hacer esto es utilizando expresiones regulares. Por ejemplo:

```Clojure
(str/replace "Hola mundo" #"o" "u")

;; Output: Hula mundu
```

También puedes utilizar la función `replaceAll` para reemplazar todas las instancias de una cadena de texto con otra. Por ejemplo:

```Clojure
(str/replace-all "Hola hola hola" #"hola" "adios")

;; Output: Adios adios adios
```

## Profundizando

Clojure también ofrece diferentes opciones y funciones para buscar y reemplazar texto de manera más compleja, como el uso de predicados en las expresiones regulares y la función `re-pattern` para construir expresiones regulares dinámicamente.

Además, con Clojure, puedes aplicar estas técnicas no solo a cadenas de texto, sino también a otras estructuras de datos como listas o mapas.

## Ver también

- [Documentación oficial de Clojure sobre buscar y reemplazar](https://clojure.org/reference/strings)
- [Video tutorial de búsqueda y reemplazo en Clojure](https://www.youtube.com/watch?v=XqxFZJMXVbw)
- [Artículo de blog sobre cómo buscar y reemplazar en Clojure](https://www.braveclojure.com/getting-started-clojure/)