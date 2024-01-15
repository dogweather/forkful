---
title:                "Buscando y reemplazando texto"
html_title:           "Clojure: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué
La búsqueda y reemplazo de texto es una habilidad esencial para cualquier programador, ya que permite hacer cambios rápidos y precisos a grandes cantidades de texto. Además, con Clojure, el proceso se vuelve aún más eficiente y elegante gracias a su enfoque funcional y su sintaxis concisa.

## Cómo hacerlo
Para realizar una búsqueda y reemplazo de texto en Clojure, podemos utilizar la función `replace` del módulo `clojure.string`. La sintaxis básica es la siguiente:

```Clojure
(require '[clojure.string :as str])

(str/replace texto-a-modificar patrón texto-de-reemplazo)
```

Veamos un ejemplo práctico. Supongamos que queremos modificar una lista de palabras eliminando las vocales. Primero, definimos la lista de palabras:

```Clojure
(def palabras ["Hola" "mundo" "en" "Clojure"])
```

Luego, utilizamos la función `replace` para eliminar todas las vocales de cada palabra:

```Clojure
(str/replace palabras #"a|e|i|o|u" "") 
; => ["Hl" "mnd" "n" "Cljr"]
```

En este ejemplo, utilizamos una expresión regular como patrón para encontrar todas las vocales (`#"[a|e|i|o|u]"`) y luego las reemplazamos por una cadena vacía (`""`).

También podemos utilizar la función `replace` para hacer cambios más complejos, como por ejemplo, cambiar el orden de las palabras de una oración. Definamos una función que haga esto:

```Clojure
(defn cambiar-orden [oracion]
  (str/replace oracion #"(\w+)\s(\w+)" "$2 $1"))
```

En este caso, estamos utilizando una expresión regular con dos grupos de captura (`(\w+)`), que representan las dos palabras de la oración. Luego, en el texto de reemplazo, utilizamos `$2` y `$1` para indicar el segundo y primer grupo de captura respectivamente, invirtiendo así el orden de las palabras.

Probemos esta función con una oración de ejemplo:

```Clojure
(cambiar-orden "Clojure es un lenguaje de programación funcional")
; => "es Clojure un de lenguaje funcional programación"
```

## Deep Dive
En Clojure, la función `replace` también acepta una función como argumento de reemplazo. Esto significa que en lugar de especificar un texto de reemplazo, podemos pasar una función que será aplicada a cada coincidencia encontrada.

Por ejemplo, si queremos reemplazar las mayúsculas por minúsculas en una cadena de texto, podemos utilizar la función `str/lower-case` como argumento de reemplazo:

```Clojure
(str/replace "Hola MUNDO" #"[A-Z]" str/lower-case)
; => "hola mundo"
```

Además, la función `replace` también acepta opciones adicionales, como `:case-insensitive` para ignorar las mayúsculas y minúsculas durante la búsqueda y `:global` para cambiar todas las coincidencias en lugar de solo la primera.

Si quieres aprender más sobre la función `replace` y otras funciones relacionadas, puedes consultar la documentación oficial de Clojure [aquí](https://clojuredocs.org/clojure.string/replace) y [aquí](https://clojure.org/api/cheatsheet).

## Ver también
- [Guía de expresiones regulares en Clojure](https://clojuredocs.org/clojure.repl/source-fn)
- [Documentación de la librería clojure.string](https://clojuredocs.org/clojure.string)
- [Tutorial de Clojure para principiantes](https://www.clojure.org/guides/getting_started)