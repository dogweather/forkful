---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elixir: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Eliminar caracteres que coinciden con un patrón es un procedimiento común en la programación. Esto permite a los programadores limpiar o transformar datos de manera efectiva.

## Cómo hacerlo:

Para hacer esto en Clojure, utilizaremos la función re-seq y la función apply str.

```Clojure
(defn borrar-patron [cadena patron]
  (apply str (re-seq (re-pattern (str "[^" patron "]")) cadena)))
```

Si ejecutamos esta función para una cadena y un patrón como el siguiente:

```Clojure
(prn (borrar-patron "Hola Mundo!" "Mundo"))
```

La salida será:

```Clojure
"Hola !"
```
Se eliminaron todos los caracteres que se encuentran en el patrón "Mundo".

## Inmersión Profunda

Historia: Clojure, un dialecto de Lisp, fue desarrollado para ser una alternativa moderna práctica. Lisp es conocido por su eficacia en el manejo de patrones y de cadenas, y Clojure sigue este legado.

Alternativas: Además de re-seq, también puedes utilizar funciones como filter y remove.

Detalles de Implementación: Cuando se usa re-seq con un patrón, se devuelve una secuencia de todas las correspondencias. Cuando esto se combina con apply str, se obtiene una cadena que excluye los caracteres del patrón.

## Ver También

Documentación oficial de Clojure para [re-seq](https://clojuredocs.org/clojure.core/re-seq)

Tutorial en español de Clojure [aquí](http://clojure-doc.org/es/articles/tutorials/introduction.html)

Un tutorial completo para [filtros](https://clojuredocs.org/clojure.core/filter) y [remove](https://clojuredocs.org/clojure.core/remove) en Clojure.