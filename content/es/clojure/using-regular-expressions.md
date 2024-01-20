---
title:                "Usando expresiones regulares"
html_title:           "Go: Usando expresiones regulares"
simple_title:         "Usando expresiones regulares"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Las expresiones regulares (RegExp por sus siglas en inglés) son patrones que se utilizan para coincidir combinaciones de caracteres en cadenas. Los programadores las usan para buscar, reemplazar y desglosar datos de manera compleja y eficiente.

## Cómo hacerlo:

Clojure proporciona una serie de funciones para trabajar con RegExp, tales como re-find, re-seq, replace, etc. Aquí hay un ejemplo simple.

```Clojure
(defn example-regexp []
  (let [pattern #"(?i)clojure"
        string "Clojure es un lenguaje de programación."]
    (re-find pattern string)))
```
Salida:
```
"Clojure"
```
En este ejemplo, "#(?i)clojure" es nuestro patrón RegExp. Esto buscará la palabra "clojure" sin importar el caso.

## Inmersión profunda:

Las RegExp se originaron en los años 50 y han sido parte de la mayoría de los lenguajes de programación. En Clojure, la api RegExp está diseñada para ser simple y eficiente. Sin embargo, existen alternativas como los operadores de secuencias si los patrones RegExp se vuelven demasiado complejos.

Las RegExp en Clojure son cautelosas y retornan la primera coincidencia que encuentran, por lo que podrías necesitar iterar sobre una cadena si buscas todas las coincidencias. También es importante recordar escapar caracteres especiales correctamente si planeas usarlos en tus patrones.

```Clojure
(defn escape-example-regexp []
  (let [pattern #"\\d+"
        string "El precio es $20."]
    (re-find pattern string)))
```
Salida:
```
"20"
```
En este ejemplo, "\\d+" es nuestro patrón RegExp que busca uno o más dígitos.

## Ver también:

Para más información, puedes referirte a la [documentación oficial de Clojure](https://clojure.org/api/api) y a las [guías de aprendizaje de Clojure](https://clojure.org/guides/getting_started), o puedes leer más sobre las [expresiones regulares en general.](https://en.wikipedia.org/wiki/Regular_expression)