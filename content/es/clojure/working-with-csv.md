---
title:                "Trabajando con archivos CSV"
html_title:           "Bash: Trabajando con archivos CSV"
simple_title:         "Trabajando con archivos CSV"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Qué es y por qué?
Trabajar con CSV significa manejar datos en formato de "valores separados por comas". Los programadores hacen esto porque es un formato de texto simple, común para importar y exportar datos de aplicaciones, bases de datos y hojas de cálculo.

## Cómo hacerlo:
Clojure facilita trabajar con CSV usando bibliotecas, como `clojure.data.csv`. Primero, instálala añadiendo `[org.clojure/data.csv "1.0.0"]` a tu archivo `project.clj`. Luego, revisa estos ejemplos.

Lee un CSV desde un archivo:
```Clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(with-open [reader (io/reader "datos.csv")]
  (doall
   (csv/read-csv reader)))
;; Output: [["columna1" "columna2"] ["dato1" "dato2"] ...]
```

Escribe un CSV a un archivo:
```Clojure
(with-open [writer (io/writer "salida.csv")]
  (csv/write-csv writer [["columna1" "columna2"] ["dato1" "dato2"]]))
;; Crea o sobrescribe salida.csv con los datos escritos
```

## Profundización:
El formato CSV existe desde la década de 1970, utilizado por programas como VisiCalc. Es accesible y editable incluso en un editor de texto. Existen alternativas más estructuradas como JSON o XML, pero CSV se mantiene ideal para tablas sencillas debido a su simplicidad. En Clojure, manipular CSV se realiza principalmente a través de la biblioteca `clojure.data.csv`, pero existen otras como `cassava` que también son populares. `clojure.data.csv` maneja los datos como listas de listas, donde cada lista interna es una fila del CSV.

## Ver también:
- Documentación de Clojure Data CSV: https://github.com/clojure/data.csv
- Para más sobre la manipulación de archivos en Clojure: https://clojuredocs.org/clojure.java.io
- Si estás buscando trabajar con JSON en lugar de CSV: https://github.com/clojure/data.json
