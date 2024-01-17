---
title:                "Eliminando caracteres que coincidan con un patrón"
html_title:           "Clojure: Eliminando caracteres que coincidan con un patrón"
simple_title:         "Eliminando caracteres que coincidan con un patrón"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Qué y por qué?

Eliminar caracteres que coinciden con un patrón es una función comúnmente utilizada en la programación para manipular y limpiar datos. Los programadores suelen hacerlo para optimizar la eficiencia del código y mejorar la legibilidad del mismo.

Cómo hacerlo:

```Clojure
(remove #"" "Texto de ejemplo con caracteres no deseados")
```

Salida: "Textodeejemploconcaracteresdeseados"

Deep Dive:

Eliminar caracteres coincidentes tiene una larga historia en la programación, siendo utilizado desde los primeros lenguajes de programación hasta la actualidad. Además, existen alternativas a esta función en otros lenguajes de programación, como la función "trim" en Java. En términos de implementación, esta función utiliza expresiones regulares para determinar los caracteres que deben ser eliminados.

Ver también:

Para más información sobre expresiones regulares y cómo utilizarlas en Clojure, revisa la documentación oficial de Clojure Regular Expressions: https://clojure.org/reference/regular_expressions. También puedes consultar el tutorial de expresiones regulares de Clojure en ClojureDocs: https://clojuredocs.org/clojure.core/re-matches.