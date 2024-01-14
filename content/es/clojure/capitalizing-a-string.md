---
title:                "Clojure: Convirtiendo una cadena en mayúsculas"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué
Hay muchas razones por las que alguien querría capitalizar una cadena de texto en Clojure. Algunos ejemplos incluyen:
- Estándares de formato: algunas empresas o equipos pueden tener reglas de formato específicas en las que todas las cadenas deben estar en mayúsculas.
- Facilitar la lectura: en algunos casos, capitalizar una cadena puede hacerla más fácil de leer y entender.
- Consistencia: puede ser una buena práctica mantener todas las cadenas en un formato consistente en tu código.

## Cómo hacerlo
Para capitalizar una cadena de texto en Clojure, puedes usar la función `clojure.string/capitalize`. Esta función toma una cadena como argumento y devuelve una nueva cadena con el primer carácter en mayúscula. Aquí hay un ejemplo de cómo se usaría esta función:

```Clojure 
(def texto "hola mundo")

(clojure.string/capitalize texto)

;; output: "Hola mundo"
```

También puedes capitalizar solo la primera letra de cada palabra en una cadena usando la función `clojure.string/capitalize-words`. Aquí hay un ejemplo:

```Clojure
(def texto "esto es una cadena de texto")

(clojure.string/capitalize-words texto)

;; output: "Esto Es Una Cadena De Texto"
```

Si quieres capitalizar solo la primera letra de una palabra, también puedes usar la función `clojure.string/capitalize-first`:

```Clojure
(def texto "hola mundo")

(clojure.string/capitalize-first texto)

;; output: "Hola mundo"
```

## Profundizando
Algunas cosas a tener en cuenta al capitalizar una cadena de texto en Clojure son:
- La función `clojure.string/capitalize` no altera la cadena original, sino que devuelve una nueva cadena.
- Si la cadena ya está en mayúsculas, la función `capitalize` no la cambiará.
- Puedes usar la función `clojure.string/upper-case` si necesitas convertir una cadena completa a mayúsculas en lugar de solo la primera letra.

## Ver también
- Documentación de Clojure para `clojure.string/capitalize`: https://clojuredocs.org/clojure.string/capitalize
- Documentación de Clojure para `clojure.string/capitalize-words`: https://clojuredocs.org/clojure.string/capitalize-words
- Documentación de Clojure para `clojure.string/capitalize-first`: https://clojuredocs.org/clojure.string/capitalize-first