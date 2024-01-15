---
title:                "Capitalizando una cadena"
html_title:           "Clojure: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Si alguna vez te has encontrado en la situación de tener una cadena de texto en minúsculas y necesitas capitalizarla, entonces esta guía es para ti. Saber cómo capitalizar una cadena en Clojure es una habilidad básica que te ayudará a manejar tus datos de manera más eficiente.

## Cómo

La función `clojure.string/capitalize` es la encargada de capitalizar una cadena de texto en Clojure. Funciona de la siguiente manera:

```
(clojure.string/capitalize "hola mundo")
;; output: "Hola mundo"
```

Esta función también puede recibir un segundo argumento opcional, que indica cuántas letras iniciales se deben capitalizar. Por defecto, sólo se capitaliza la primera letra de la cadena:

```
(clojure.string/capitalize "hola mundo" 4)
;; output: "HOLA mundo"
```

## Profundizando

Internamente, la función `capitalize` utiliza la función `freq` para obtener la frecuencia de cada letra en la cadena. Luego, utiliza la función `apply` con la función `str` para concatenar la cadena capitalizada.

```
(defn capitalize
  "A version of str that applies to the first character of each word in
  the supplied string."
  {:added "1.1"}
  ([s]
   (apply str (conj (map (fn [word]
                           (apply str (map (fn [[idx char]] (if (zero? idx)
                                                                (Character/toTitleCase char)
                                                                (Character/toLowerCase char)))
                                           (frequencies word)))))
                     s)))
  ([s n] (clojure.string/join " " (percent-remainders/percent [1]
                         (re-seq #"[^ ]+" s) n))))
```

## Ver también

* Documentación oficial de `clojure.string/capitalize`: https://clojuredocs.org/clojure.string/capitalize
* Más funciones útiles para trabajar con cadenas de texto en Clojure: https://clojuredocs.org/clojure.string