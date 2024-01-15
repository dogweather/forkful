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

## Por qué

Trabajar con archivos CSV puede ser muy útil cuando se trata de analizar grandes conjuntos de datos. Además, es una forma sencilla y conveniente de guardar y compartir datos en formato tabular.

## Cómo hacerlo

Para trabajar con archivos CSV en Clojure, primero necesitamos importar la librería "clojure.data.csv". Luego, podemos utilizar la función "csv/read" para leer un archivo CSV y convertirlo en una secuencia de mapas en donde cada mapa representa una fila del archivo.

```
(require '[clojure.data.csv :as csv])

(def csv-data (csv/read "datos.csv"))

;; csv-data => ({:nombre "Ana" :edad 30 :ciudad "Madrid"}
;;               {:nombre "Juan" :edad 25 :ciudad "Barcelona"}
;;               {:nombre "Sofía" :edad 33 :ciudad "Valencia"})
```

Podemos seleccionar columnas específicas utilizando la función "csv/cut" y aplicar funciones de agregación a los datos utilizando "csv/select". Por ejemplo, podemos obtener la suma de todas las edades en nuestro conjunto de datos:

```
(csv/select (csv/cut csv-data [:edad]) :sum)

;; output => {:edad 88}
```

También podemos utilizar la función "csv/write" para escribir datos en formato CSV a un archivo.

```
(csv/write [["Rodrigo" 27 "Sevilla"] ["Laura" 29 "Bilbao"]] "nuevos_datos.csv")
```

Esto generará un nuevo archivo CSV con los datos proporcionados.

## Profundizando

Además de las funciones mencionadas anteriormente, la librería "clojure.data.csv" ofrece una variedad de herramientas para trabajar con archivos CSV, como funciones para filtrar, ordenar y combinar datos, así como opciones para personalizar el formato de escritura de datos. También podemos utilizar la librería "clojure.java.jdbc" para conectarnos y escribir datos a una base de datos a partir de un archivo CSV.

## Ver también

- [Documentación oficial de Clojure sobre la librería "clojure.data.csv"](https://clojure.github.io/data.csv/)
- [Tutorial sobre cómo trabajar con archivos CSV en Clojure](https://www.infoq.com/articles/clojure-csv-data-analytics/)