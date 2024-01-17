---
title:                "Interpolando una cadena"
html_title:           "Clojure: Interpolando una cadena"
simple_title:         "Interpolando una cadena"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Interpolar una cadena en Clojure significa insertar una o más variables dentro de una cadena de caracteres para crear una cadena final con valores específicos de esas variables. Los programadores lo hacen para crear cadenas dinámicas y personalizadas que contienen información específica en diferentes contextos.

## Cómo hacerlo:

Para interpolar una cadena en Clojure, use la función `format` y especifique la cadena de formato seguida de las variables que desea insertar entre paréntesis.

```
Clojure (format "Hola %s, ¿cómo estás?" "Juan")

; output: "Hola Juan, ¿cómo estás?"
```

También puede usar la sintaxis de cadena de interpolación con `` o la función `str`.

```
Clojure `(Hola ${nombre}, ¿cómo estás?)

; output: "Hola Juan, ¿cómo estás?"

Clojure (str "Hola " nombre ", ¿cómo estás?")

; output: "Hola Juan, ¿cómo estás?"
```

## Profundizando:

Interpolar cadenas ha sido una técnica común en muchos lenguajes de programación durante mucho tiempo. Sin embargo, en Clojure, se alienta a los programadores a utilizar el concepto de "créditos léxicos" en su lugar, evitando así el uso del formato de cadena tradicional. Esto permite una mejor separación de código y datos, lo que a su vez puede mejorar el rendimiento y la comprensión del código. Alternativas como `str` y `print` también pueden ser más eficientes para crear cadenas en lugar de `format`.

## Ver también:

- [Sintaxis de cadena de interpolación](https://clojure.org/reference/reader#_string_interpolation)
- [Documentación de `format`](https://clojuredocs.org/clojure.core/format)
- [Artículo sobre créditos léxicos en Clojure](https://coderwall.com/p/4fcreg/don-t-use-format-use-sequential-types)