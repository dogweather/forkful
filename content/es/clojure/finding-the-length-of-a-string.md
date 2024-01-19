---
title:                "Encontrando la longitud de una cadena"
html_title:           "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Encontrar la Longitud de una Cadena en Clojure

## ¿Qué y Por Qué?

Encontrar la longitud de una cadena es determinar el número de caracteres en una cadena dada. Los programadores a menudo necesitan esta información para controlar los flujos de datos y para operaciones de manipulación de cadenas.

## Cómo hacerlo:

En Clojure, usaríamos la función `count` para encontrar la longitud de una cadena. Aquí hay un ejemplo:

```clojure
(def cadena "Hola, Mundo!")
(print "La longitud de la cadena es: " (count cadena))
```

La salida de este código sería:

```
La longitud de la cadena es: 13
```

## Deep Dive:

1. Contexto histórico: Clojure es un dialecto de Lisp y comparte con este la prioridad de tratar el código como datos y tener una sintaxis Unicode. Aunque Lisp tradicionalmente no enfatiza las cadenas, Clojure, que se construyó para interopera con la Máquina Virtual de Java (JVM), consideró crucial una funcionalidad sólida de cadenas.

2. Alternativas: Puedes usar Java Interop para acceder al método `length` de una cadena de Java:

```clojure
(.length "Hola, Mundo!")
```

Esto daría el mismo resultado que la función `count`. Sin embargo, esta opción es menos idiomatica y más orientada hacia Java que la función `count`.

3. Detalles de la implementación: En Clojure, `count` es una función multiplataforma, lo que significa que puede operar en varias colecciones además de las cadenas. Cuando se aplica a una cadena, llama la función de longitud de la clase String en Java. Tras bambalinas, esta función se implementa mediante el conteo de unidades de código Unicode, lo que puede dar resultados inesperados con algunos caracteres especiales.

## Véase También:

Para más detalles sobre las cadenas y su manipulación en Clojure, consulta estos recursos:

1. [Clojure Strings - Clojure Documentation](https://clojure-doc.org/articles/tutorials/strings.html)
2. [Strings and Characters - Clojure Programming Cookbook](https://www.oreilly.com/library/view/clojure-programming-cookbook/9781785885037/ch01s04.html)
3. [Clojure from the ground up: basic types](https://aphyr.com/posts/302-clojure-from-the-ground-up-basic-types)
4. [Clojure for the Brave and True: Learn the Ultimate Language and Become a Better Programmer](https://www.braveclojure.com/)