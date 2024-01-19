---
title:                "Buscando y reemplazando texto"
html_title:           "C: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Buscar y reemplazar texto es un método para localizar cadenas de texto específicas en un cuerpo de texto y cambiarlas por algo diferente. Los programadores lo hacen para manipular datos, corregir errores o adaptar el contenido a nuevas situaciones.

## ¿Cómo se hace?

En Clojure, basta con usar el método `clojure.string/replace`. 

Por ejemplo, vamos a reemplazar todas las apariciones de la cadena "Hola" por "Adiós" en un texto:

```clojure
(require '[clojure.string :as str])

(def text "Hola, Mundo. Hola, Programador.")
(def new-text (str/replace text "Hola" "Adiós"))
```

La salida de este código sería: 

```clojure
"Adiós, Mundo. Adiós, Programador."
```

## Un vistazo profundo

Buscar y reemplazar textos es tan antiguo como la misma informática. Ya desde los primeros editores de texto, se ha usado para facilitar la edición y manipulación de datos. 

Existen muchas alternativas al método `str/replace`. Puedes usar `str/replace-first` si sólo quieres reemplazar la primera coincidencia. Además, Clojure permite el uso de expresiones regulares para buscar patrones de texto más complejos.

El corazón de la implementación de `str/replace` en Clojure es el API de Java `StringBuilder` y `String`. Cada coincidencia es localizada y el texto es reconstruido con las sustituciones requeridas.

## Consulta También

Para más detalles sobre la manipulación de strings en Clojure, consulta la [documentación oficial](https://clojure.github.io/clojure/clojure.string-api.html). Si quieres un reto, intenta implementar tu propia función de buscar y reemplazar usando los fundamentos de Clojure. Recomendamos [esta guía](https://clojuredocs.org/clojure.core/loop) sobre cómo utilizar el loop y recur para la creación de funciones personalizas.