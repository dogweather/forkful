---
title:    "Clojure: Utilizando expresiones regulares"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## ¿Por qué usar expresiones regulares en Clojure?

Las expresiones regulares son una forma poderosa y eficiente de buscar y manipular cadenas de texto en un programa. Se pueden utilizar en Clojure para realizar tareas como validar entradas de usuario, extraer información de un texto o transformar datos en un formato determinado. Si bien pueden parecer intimidantes al principio, una vez que comprenda cómo funcionan, se dará cuenta de la utilidad que tienen en su código.

## Cómo usar expresiones regulares en Clojure

Para utilizar expresiones regulares en Clojure, necesitaremos importar el módulo `java.util.regex`, que contiene las clases y métodos necesarios. Luego, podemos utilizar la función `re-find` para buscar una coincidencia en una cadena de texto utilizando una expresión regular.

```Clojure
(require '[java.util.regex :as regex])

(re-find #"\d+" "La edad de Pedro es 25") ; salida: "25"
```

En este ejemplo, utilizamos la expresión regular `\d+` para encontrar una serie de dígitos en la cadena de texto "La edad de Pedro es 25". También podemos utilizar la función `re-seq` para buscar todas las coincidencias en una cadena y devolver una secuencia con ellas.

```Clojure
(re-seq #"([A-Z]\w+)" "Mi nombre es Juan Pablo") ; salida: ("Mi" "Juan" "Pablo")
```

Podemos utilizar diferentes operadores y símbolos en nuestras expresiones regulares para crear patrones más complejos. Por ejemplo, el símbolo `+` indica que el elemento anterior debe aparecer una o más veces en la cadena, mientras que `*` indica que puede aparecer cero o más veces. Además, podemos utilizar los caracteres especiales `^` y `$` para indicar el inicio y el final de una cadena, respectivamente.

## Profundizando en el uso de expresiones regulares

Uno de los aspectos más importantes de las expresiones regulares es la capacidad de agrupar patrones y utilizarlos en conjunto. Esto se puede lograr utilizando paréntesis en nuestra expresión regular. Por ejemplo, podemos utilizar la expresión `(\d+)(\w+)` para buscar una serie de dígitos seguida de una serie de letras en una cadena de texto.

Otro concepto clave es el de los "grupos de captura". Estos son patrones dentro de una expresión regular que pueden ser referenciados y utilizados en el resultado de la búsqueda. Por ejemplo, en la expresión `([A-Z]\w+)`, el grupo de captura es `([A-Z])` y podemos hacer referencia a él utilizando el número correspondiente al grupo en el resultado de la búsqueda.

## Ver también

- Documentación oficial de expresiones regulares en Clojure: https://clojure.org/reference/regular_expressions
- Una guía completa de expresiones regulares en Clojure: https://clojuredocs.org/clojure.core/re-find
- Ejemplos de uso de expresiones regulares en Clojure: https://clojuredocs.org/clojure.repl/doc-docstring