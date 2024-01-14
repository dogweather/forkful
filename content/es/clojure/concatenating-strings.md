---
title:                "Clojure: Uniendo cadenas"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué
La concatenación de cadenas es una herramienta fundamental en cualquier lenguaje de programación, incluyendo Clojure. Te permite unir varias cadenas para crear una nueva y manipular datos de forma más eficiente. 

## Cómo hacerlo
En Clojure, la concatenación de cadenas se puede realizar de varias maneras. Una forma es utilizar la función `str`, que toma cualquier número de argumentos de cadena y los combina en una sola cadena.

```Clojure
(str "Hola" "mundo") ; la salida será "Holamundo"
```

También puedes usar la función `str` para unir cadenas con otros tipos de datos, como números y otros valores.

```Clojure
(str "El número es " 42) ; la salida será "El número es 42"
```

Otra forma de concatenar cadenas en Clojure es utilizando la función `format`, que te permite crear una cadena con formato. Esto significa que puedes especificar dónde quieres que se inserten los valores dentro de la cadena.

```Clojure
(format "Hola %s" "Juan") ; la salida será "Hola Juan"
(format "La respuesta es %d" 42) ; la salida será "La respuesta es 42"
```

## Profundizando
Clojure también ofrece otras funciones para la manipulación de cadenas, como `subs`, que te permite obtener una subcadena de una cadena dada.

```Clojure
(subs "Hola mundo" 0 4) ; la salida será "Hola"
```

También puedes utilizar la función `join` para unir varias cadenas en una sola, utilizando un separador opcional.

```Clojure
(join ", " ["manzana" "plátano" "naranja"]) ; la salida será "manzana, plátano, naranja"
```

## Ver también
- [Documentación oficial de Clojure](https://clojure.org/guides/strings)
- [Tutorial de concatenación de cadenas en Clojure](https://www.tutorialspoint.com/clojure/clojure_strings.htm)
- [Ejemplos prácticos de concatenación de cadenas en Clojure](https://www.javacodegeeks.com/2019/04/10-clojure-examples-concatenating-strings.html)