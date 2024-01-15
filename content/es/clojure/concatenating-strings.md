---
title:                "Concatenando cadenas"
html_title:           "Clojure: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

##Por qué

¡Hola! Si estás leyendo esto, probablemente estás interesado en aprender sobre concatenar strings en Clojure. Este es un concepto básico pero muy útil en la programación, ya que te permite unir varias cadenas de texto en una sola.

##Cómo Hacerlo

Para concatenar strings en Clojure, podemos usar la función "str", que toma cualquier cantidad de argumentos y los concatena en una sola cadena. Por ejemplo, si queremos unir las cadenas "¡Hola" y "mundo!", podemos escribir:

```Clojure
(str "¡Hola" "mundo!")
```

Esto daría como resultado la cadena "¡Hola mundo!".

También podemos usar la función "str" para concatenar cadenas con otros datos, como números. Por ejemplo, si queremos tener una cadena que diga "El resultado es 10", podemos escribir:

```Clojure
(str "El resultado es" 10)
```

Esto nos daría como resultado la cadena "El resultado es 10".

##Profundizando

¿Sabías que también puedes usar el operador "+" para concatenar strings en Clojure? Este operador suma cualquier tipo de dato que le pasemos, incluyendo strings.

Por ejemplo, podemos escribir:

```Clojure
(+ "¡Hola " "mundo!")
```

Esto también daría como resultado la cadena "¡Hola mundo!". Sin embargo, debes tener en cuenta que solo puedes usar el operador "+" con dos argumentos, mientras que la función "str" puede tomar cualquier cantidad.

¡Ahora ya sabes cómo concatenar strings en Clojure! Recuerda que esto es solo una de las muchas funcionalidades de este poderoso lenguaje de programación funcional.

##Ver también

- [Documentación oficial de Clojure sobre la función "str"](https://clojuredocs.org/clojure.core/str)
- [Guía de referencia rápida de Clojure](https://clojure.org/api/cheatsheet)