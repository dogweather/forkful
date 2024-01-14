---
title:    "Clojure: Extrayendo subcadenas"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por qué
La extracción de subcadenas es una técnica esencial para trabajar con cadenas de texto en cualquier lenguaje de programación, incluyendo Clojure. Permite obtener partes específicas de una cadena de texto, lo que es útil para realizar diversas tareas, como la manipulación de texto, la validación de datos y la búsqueda de patrones.

## Cómo hacerlo
Para extraer una subcadena en Clojure, podemos utilizar la función `subs`. Esta función toma una cadena de texto y dos índices numéricos como argumentos. El primer índice representa el inicio de la subcadena (incluyendo el carácter correspondiente), mientras que el segundo índice representa el final de la subcadena (excluyendo el carácter correspondiente). Por ejemplo, si queremos extraer la subcadena "mundo" de la cadena "Hola mundo", podemos escribir:

```Clojure
(subs "Hola mundo" 5 10)
```

Esto devolverá la subcadena "mundo". También podemos utilizar índices negativos para contar desde el final de la cadena. Por ejemplo, si queremos extraer la última palabra de la misma cadena, podemos escribir:

```Clojure
(subs "Hola mundo" -5)
```

Esto devolverá la subcadena "mundo". Finalmente, si queremos extraer una parte de una cadena de forma dinámica, podemos utilizar la función `subs` junto con variables en lugar de valores numéricos.

## Profundizando
Es importante tener en cuenta que la función `subs` en Clojure es "sin estado", lo que significa que no altera la cadena original, sino que devuelve una nueva cadena con la subcadena extraída. Además, esta función también puede ser utilizada en vectores y secuencias, no solo en cadenas de texto.

También es posible extraer subcadenas utilizando expresiones regulares en Clojure. Para ello, podemos utilizar la función `re-find` y pasar como argumento una expresión regular que coincida con la parte de la cadena que queremos extraer.

## Ver también
- [Documentación oficial de Clojure sobre la función `subs`](https://clojuredocs.org/clojure.core/subs)
- [Explicación detallada sobre la extracción de subcadenas en Clojure](https://codurance.com/2019/06/13/substring-in-clojure/) (en inglés)
- [Ejemplos prácticos de la extracción de subcadenas en Clojure](https://notanumber.io/2015/substring-from-string-in-content_in_clojure-the-correct/) (en inglés)