---
title:                "Utilizando expresiones regulares"
html_title:           "Clojure: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Por qué

Si estás trabajando con texto en tus programas de Clojure, es muy probable que en algún momento necesitarás buscar patrones o realizar operaciones de búsqueda y reemplazo. Es ahí donde entran en juego las expresiones regulares, una herramienta poderosa para trabajar con cadenas de texto.

## Cómo hacerlo

Para utilizar expresiones regulares en Clojure, primero necesitamos importar la biblioteca `java.util.regex`. Luego, podemos utilizar la función `re-seq` para buscar todas las coincidencias de una expresión regular en una cadena de texto:

```Clojure
(require '[java.util.regex :as re])

(def texto "Hola, soy un texto de ejemplo!")
(re-seq #"soy" texto)
; => ("soy")
```

También podemos utilizar la función `re-matches` para verificar si una cadena de texto coincide con una expresión regular:

```Clojure
(re-matches #"texto" texto)
; => "texto"
```

Otra función útil es `re-find`, que devuelve la primera coincidencia de una expresión regular en una cadena de texto:

```Clojure
(re-find #"\w+" texto)
; => "Hola"
```

También podemos realizar operaciones de búsqueda y reemplazo utilizando la función `re-find` junto con `substitute`:

```Clojure
(re-find #"\d+" "Tengo 10 años.")
;(substitute "21" "Tengo ${n} años." %)
; => "Tengo 21 años."
```

## Profundizando

Las expresiones regulares son un lenguaje propio con su propia sintaxis y reglas. Algunos patrones comunes incluyen `.*` para representar cualquier caracter, `\w` para representar letras y números, y `\d` para representar solo números. También podemos utilizar `^` para indicar que la cadena debe comenzar con cierto patrón y `$` para indicar que debe terminar con cierto patrón.

Además, es posible utilizar grupos de captura utilizando paréntesis para luego acceder a ellos en las operaciones de reemplazo. Por ejemplo:

```Clojure
(re-find #"(\w+) (\w+)" "John Doe")
;(substitute "$2, $1" "${1} ${2}" %)
; => "Doe, John"
```

Otra característica interesante es la posibilidad de utilizar modificadores para aplicar diferentes lógicas en nuestras expresiones regulares. Algunos de los modificadores disponibles incluyen `i` para ignorar mayúsculas y minúsculas, `g` para aplicar a todas las coincidencias y `m` para tratar la cadena como una cadena de varias líneas.

## Ver también

- Documentación oficial de expresiones regulares en Clojure: https://clojure.org/guides/learn/regular_regexes
- Tutorial interactivo de RegexOne para aprender más sobre expresiones regulares: https://regexone.com/