---
title:                "Extrayendo subcadenas"
html_title:           "Clojure: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué
Extraer subcadenas es una habilidad fundamental en la programación, especialmente en Clojure. Es importante saber cómo hacerlo para poder manipular y analizar cadenas de texto de manera eficiente.

## Cómo hacerlo
En Clojure, hay algunas formas diferentes de extraer subcadenas, dependiendo de lo que necesite en su programa. A continuación presentamos algunos ejemplos utilizando la función `subs`:

```Clojure
;; Extraer los primeros 5 caracteres de una cadena
(subs "Hola mundo" 0 5)
;; Salida: "Hola "

;; Extraer los últimos 3 caracteres de una cadena
(subs "Esta es una cadena" (- (count "Esta es una cadena") 3))
;; Salida: "na"

;; Extraer una subcadena entre dos índices específicos
(subs "¡Hola a todos!" 1 8)
;; Salida: "Hola a "
```

También puede utilizar la función `subseq`, que toma una secuencia como primer argumento en lugar de una cadena. Esto le permite extraer subcadenas de secuencias más complejas, como vectores o listas.

```Clojure
;; Extraer una subcadena de un vector
(subseq ["Esta" "es" "una" "prueba"] 0 2)
;; Salida: ["Esta" "es"]

;; Extraer una subcadena de una lista
(subseq (list 1 2 3 4 5 6) 2 5)
;; Salida: (3 4 5)
```

## Profundizando
La función `subs` en realidad utiliza `reduce` para manejar la extracción de subcadenas de manera más eficiente. Por ejemplo, si desea extraer una subcadena larga de una cadena, la función `reduce` dividirá la cadena en trozos más pequeños y después los concatenará para obtener la subcadena deseada.

También puede utilizar `subs` para realizar operaciones de filtrado en sus datos. Por ejemplo, si desea seleccionar solo las vocales de una cadena, puede utilizar `subs` junto con la función `filter` de la siguiente manera:

```Clojure
(filter #{\a\e\i\o\u} (subs "Este es un ejemplo de filtrado de vocales"))
;; Salida: (\e \e u \i o \a u a o \i o)
```

## Ver también
- [Documentación oficial de Clojure para la función `subs`](https://clojuredocs.org/clojure.core/subs)
- [Tutorial de Clojure para extracción de subcadenas](https://www.clojure-doc.org/articles/tutorials/strings.html)
- [Ejemplos prácticos de extracción de subcadenas en Clojure](http://thegeez.github.io/misc/clojure/strings/2013/06/04/clojure-string-magic.html)