---
title:                "Trabajando con csv"
html_title:           "Clojure: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué se usa?

Trabajar con CSV (Comma Separated Values) significa manipular datos que están organizados en una tabla, donde cada columna está separada por una coma. La mayoría de los programadores trabajan con CSV debido a su facilidad de uso y flexibilidad en la manipulación de datos.

## Cómo hacerlo:

Para trabajar con CSV en Clojure, podemos utilizar la librería "clojure.data.csv". Primero, debemos importarla:

`Clojure (require '[clojure.data.csv :as csv])`

Luego, podemos utilizar la función "with-csv-reader" para leer un archivo CSV y convertir sus datos a una lista:

```
Clojure (with-csv-reader
  (io/reader "data.csv")
  (fn [line] (println line)))
```

Esta función ejecutará la función que le pasamos como segundo argumento en cada línea del archivo, imprimiendo cada línea en la consola. También podemos utilizar la función "parse-csv" para convertir los datos del archivo en una lista de listas, donde cada lista representa una fila de la tabla:

```
Clojure (->> (with-open [r (io/reader "data.csv")]
                      (parse-csv r))
           (map #(for [i (range (count %))]
                         ({(get-in % [0 i]) (nth % (inc i))}))))
```

Este código primero abre el archivo y lo convierte en una lista de listas utilizando la función "parse-csv". Luego, utilizamos "map" para recorrer cada lista y crear un mapa con las columnas como claves y los datos correspondientes como valores.

## Profundizando:

El formato CSV se ha vuelto muy popular en la manipulación de datos debido a su simplicidad y flexibilidad. Sin embargo, también existen otros formatos como JSON o XML que también son utilizados para almacenar y compartir datos estructurados. En Clojure, existen diferentes librerías para trabajar con estos formatos, como "clojure.data.json" o "clojure.data.xml".

En cuanto a la implementación de la manipulación de CSV en Clojure, esta se realiza utilizando la librería "clojure.data.csv" que utiliza la función "parse-csv" para convertir los datos del archivo en una secuencia de secuencias. Luego, podemos utilizar funciones de Clojure como "map" o "filter" para manipular estos datos según nuestras necesidades.

## Ver también:

- Documentación oficial de la librería "clojure.data.csv"
- Ejemplos de manipulación de CSV en Clojure en el sitio web "4clojure.com"
- Librería "clojure.data.json" para trabajar con el formato JSON en Clojure