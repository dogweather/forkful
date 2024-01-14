---
title:                "Clojure: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

-------
# ¿Por qué utilizar expresiones regulares en Clojure?

Si eres un programador interesado en el procesamiento de texto y la manipulación de datos, entonces las expresiones regulares son una herramienta imprescindible en tu caja de herramientas. Con las expresiones regulares, puedes encontrar patrones específicos de texto en un conjunto de datos de manera rápida y eficiente. En Clojure, la manipulación de expresiones regulares es especialmente conveniente debido a la implementación de la librería `java.util.regex`.

## Cómo utilizar expresiones regulares en Clojure

Para utilizar expresiones regulares en Clojure, primero debes importar la librería `java.util.regex` utilizando la función `import`.

```Clojure
(import 'java.util.regex.Pattern)
```

Luego, puedes crear un objeto Pattern para tu expresión regular utilizando la función `compile`. Por ejemplo, si deseas buscar todas las coincidencias de la palabra "Hola" en una cadena de texto, puedes utilizar la siguiente expresión regular:

```Clojure
(def patron (Pattern/compile "Hola"))
```

Ahora, puedes usar el método `matcher` para encontrar todas las coincidencias en una cadena de texto dada utilizando la función `find` y mostrando el resultado en una lista de Clojure utilizando la función `re-seq`.

```Clojure
(def texto "Hola, ¿cómo estás? Hola, espero que estés bien.")
(re-seq (find patron texto))
```

La salida será una lista de dos elementos, ya que hay dos coincidencias de la palabra "Hola" en el texto:

```Clojure
("Hola" "Hola")
```

## Profundizando en el uso de expresiones regulares

Una de las ventajas de usar expresiones regulares en Clojure es la capacidad de hacer coincidir patrones más complicados utilizando ciertas secuencias de caracteres especiales. Algunos ejemplos comunes incluyen:

- `.`: coincide con cualquier carácter
- `+`: coincide con uno o más repeticiones del carácter anterior
- `*`: coincide con cero o más repeticiones del carácter anterior
- `?`: coincide con cero o una repetición del carácter anterior
- `[]`: coincide con cualquier carácter dentro de los corchetes
- `^`: coincide con el inicio de una línea de texto
- `$`: coincide con el final de una línea de texto

Por ejemplo, si quisieras encontrar todas las direcciones de correo electrónico en un texto, podrías utilizar la siguiente expresión regular:

```Clojure
(def patron (Pattern/compile "[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+"))
```

## Ver también

- [Documentación sobre el uso de expresiones regulares en Clojure](https://clojuredocs.org/clojure.core/re-seq)
- [Expresiones regulares en Java](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [Ejemplos de expresiones regulares en Clojure](https://github.com/justonemorefix/clojure-regex-examples)