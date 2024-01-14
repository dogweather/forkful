---
title:                "Clojure: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Por qué trabajar con CSV?

CSV (Comma Separated Values) es un formato muy utilizado para almacenar datos tabulares en un archivo de texto simple. Este formato es ampliamente compatible y fácil de entender, lo que lo hace ideal para compartir datos entre diferentes aplicaciones. En este artículo, te mostraremos cómo trabajar con CSV en Clojure y algunos consejos para aprovechar al máximo este formato de archivo.

## Cómo hacerlo

En Clojure, podemos usar la librería `clojure.data.csv` para leer y escribir archivos CSV. Primero, importamos la librería en nuestro código:

```Clojure
(require '[clojure.data.csv :as csv])
```

Luego, podemos leer un archivo CSV utilizando la función `csv/read-csv` y especificando la ruta del archivo como argumento:

```Clojure
(def csv-file (csv/read-csv "ruta/al/archivo.csv"))
```

Este código devolverá una secuencia con los datos del archivo CSV. Podemos imprimirlo para ver la salida:

```Clojure
(println csv-file)
;; => (("Nombre" "Edad" "Ciudad")
;;     ("Maria" "25" "Madrid")
;;     ("Juan" "32" "Barcelona")
;;     ("Sofia" "28" "Valencia"))
```

También podemos escribir datos en un archivo CSV utilizando la función `csv/write-csv` y especificando la ruta y los datos como argumentos:

```Clojure
(csv/write-csv "ruta/al/nuevo-archivo.csv" [["Nombre" "Email"]
                                           ["Maria" "maria@email.com"]
                                           ["Juan" "juan@email.com"]])
```

Este código creará un nuevo archivo CSV con los datos proporcionados. Ahora, `new-csv-file` contendrá:

```Clojure
[("Nombre" "Email")
 ("Maria" "maria@email.com")
 ("Juan" "juan@email.com")]
```

## Deep Dive

La librería `clojure.data.csv` también ofrece varias opciones para personalizar la forma en que manejamos los datos CSV. Por ejemplo, podemos especificar un separador diferente al predeterminado (`,`), agregar una fila de encabezado a nuestros datos, o incluso incluir un encabezado a medida para cada columna.

Además, al leer un archivo CSV, podemos especificar un transformador de lectura para manipular los datos según nuestras necesidades. Por ejemplo, podemos convertir los datos en un mapa utilizando la función `csv/keyed-read-csv`, especificando la columna que queremos como clave.

## Ver también

Para más información sobre cómo trabajar con CSV en Clojure, te recomendamos revisar la documentación oficial de `clojure.data.csv` y leer algunos ejemplos de código en línea:

- Documentación de clojure.data.csv: https://clojure.github.io/data.csv/
- Ejemplo de lectura y escribir archivos CSV: https://www.baeldung.com/clojure-read-write-csv