---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:03.050226-07:00
description: "Trabajar con archivos CSV (Valores Separados por Comas) implica analizar\
  \ y generar datos de texto estructurados como filas y columnas, similar a los datos\u2026"
lastmod: 2024-02-19 22:05:17.270140
model: gpt-4-0125-preview
summary: "Trabajar con archivos CSV (Valores Separados por Comas) implica analizar\
  \ y generar datos de texto estructurados como filas y columnas, similar a los datos\u2026"
title: Trabajando con CSV
---

{{< edit_this_page >}}

## Qué y Por Qué

Trabajar con archivos CSV (Valores Separados por Comas) implica analizar y generar datos de texto estructurados como filas y columnas, similar a los datos de una hoja de cálculo. Este proceso es esencial para el intercambio de datos entre aplicaciones, bases de datos, y para tareas de transformación de datos, debido a la amplia adopción del CSV como un formato ligero e interoperable.

## Cómo:

### Leyendo un Archivo CSV
Clojure no tiene análisis de CSV incorporado en su biblioteca estándar, pero puedes usar la biblioteca `clojure.data.csv` para este propósito. Primero, añade la biblioteca a las dependencias de tu proyecto.

En tu `project.clj`, añade la siguiente dependencia:
```clojure
[clojure.data.csv "1.0.0"]
```
Para leer un archivo CSV e imprimir cada fila:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(with-open [reader (io/reader "ruta/a/tuarchivo.csv")]
  (doall
   (map println (csv/read-csv reader))))
```
Esto mostrará cada fila del CSV como un vector de Clojure.

### Escribiendo en un Archivo CSV
Para escribir datos en un archivo CSV, puedes usar la misma biblioteca `clojure.data.csv`:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(let [datos [["id" "nombre" "edad"]
            ["1" "John Doe" "28"]
            ["2" "Jane Doe" "31"]]]
  (with-open [writer (io/writer "ruta/a/archivodesalida.csv")]
    (csv/write-csv writer datos)))
```
Esto crea o sobrescribe `archivodesalida.csv`, llenándolo con los datos especificados.

### Usando una Biblioteca de Terceros: `clojure.data.csv`

Aunque `clojure.data.csv` es posiblemente la biblioteca más sencilla para manejar CSV en Clojure, para tareas más complejas, como manejar CSVs con caracteres especiales o delimitadores no convencionales, podrías explorar opciones adicionales dentro del ecosistema o incluso considerar interoperabilidad con Java con bibliotecas como Apache Commons CSV. Sin embargo, para la mayoría de las tareas estándar de procesamiento de CSV en Clojure, `clojure.data.csv` proporciona un conjunto de herramientas simple y efectivo.
