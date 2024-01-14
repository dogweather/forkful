---
title:                "Clojure: Extrayendo subcadenas"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

# Por qué extraer subcadenas en Clojure

En la programación, a menudo nos encontramos con la necesidad de extraer una parte de una cadena o un texto para realizar ciertas operaciones. En Clojure, la extracción de subcadenas nos permite manipular y trabajar con estos fragmentos de texto de manera eficiente.

## Cómo hacerlo

Extraer subcadenas en Clojure es bastante sencillo y se puede realizar de varias maneras. La forma más básica es utilizar la función `subs`, que toma como parámetros una cadena, el índice de inicio y el índice final de la subcadena que queremos extraer.

```Clojure
(def cadena "Hola mundo")
(subs cadena 0 4)
```
*Salida: Hola*

También podemos utilizar la función `substring`, que tiene una sintaxis similar a `subs` pero nos permite omitir el índice final si queremos extraer desde el inicio hasta el final de la cadena.

```Clojure
(def cadena "Hola mundo")
(substring cadena 5)
```
*Salida: mundo*

Para realizar extracciones más complejas, podemos utilizar expresiones regulares con la función `re-find`. Esta función toma como parámetros una expresión regular y una cadena, y devolverá la primera coincidencia encontrada en la cadena.

```Clojure
(def cadena "Hola mundo")
(re-find #"mundo" cadena)
```
*Salida: mundo*

## Profundizando

La extracción de subcadenas es una herramienta muy útil en Clojure, ya que nos permite trabajar con fragmentos de texto de manera eficiente y sencilla. Además de las funciones mencionadas anteriormente, también podemos utilizar las funciones `subs`, `substring` y `re-find` junto con otras funciones de manipulación de cadenas en combinaciones más complejas para obtener resultados aún más precisos.

Otra opción es utilizar la librería `clojure.string`, que contiene diversas funciones para trabajar con cadenas, incluyendo la función `replace`, que nos permite reemplazar fragmentos de texto en una cadena con otro texto.

## Véase también

- [Documentación de Clojure sobre la extracción de subcadenas](https://clojure.org/reference/strings)
- [Tutorial sobre expresiones regulares en Clojure](https://clojure.org/guides/regular_expressions)