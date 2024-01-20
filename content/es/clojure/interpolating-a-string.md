---
title:                "Interpolando una cadena de texto"
html_title:           "Haskell: Interpolando una cadena de texto"
simple_title:         "Interpolando una cadena de texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

La interpolación de strings es un proceso para insertar variables en un string. Los programadores lo hacen para crear strings más dinámicos y legibles.

## Cómo Hacerlo:

La interpolación de strings en Clojure se hace generalmente usando la función `format`, similar a `printf` de C. Aquí tienes un ejemplo básico:

```clojure
(let [nombre "Juan"]
  (format "Hola, %s" nombre))
```

Resultado:

```clojure
"Hola, Juan"
```

## Conociendo Más:

**Historia:** La interpolación de strings proviene de los primeros lenguajes de programación como ALGOL y ha sido adoptado por la mayoría de los lenguajes modernos.

**Alternativas:** Puedes usar concatenación de strings (`str` en Clojure) pero suele ser menos legible cuando trabajas con múltiples variables.

```clojure
(let [nombre "Juan"]
  (str "Hola, " nombre))
```

**Detalles de Implementación:** `format` en Clojure es una función variádica que acepta cualquier número de argumentos después de la cadena de formato. Estos argumentos se insertan en la cadena de caracteres de formato en los lugares donde se encuentran los especificadores de formato como `%s` para strings.

## Ver También:

1. Documentación oficial de Clojure: [format](https://clojuredocs.org/clojure.core/format)
2. Página de Clojure de Wikipedia: [Clojure](https://es.wikipedia.org/wiki/Clojure)
3. Guía práctica de Clojure: [Aprendiendo Clojure](https://www.learn-clojure.com/)
4. `str` vs `format` en Clojure: [Debate en StackOverflow](https://stackoverflow.com/questions/46275278/string-concatenation-vs-format-performance-in-clojure)