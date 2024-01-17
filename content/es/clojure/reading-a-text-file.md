---
title:                "Leyendo un archivo de texto"
html_title:           "Clojure: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Leer un archivo de texto es una tarea común en la programación que consiste en obtener el contenido de un archivo de texto y almacenarlo en memoria para su posterior procesamiento. Los programadores realizan esta tarea para trabajar con datos almacenados en archivos, como configuraciones de software o registros de usuarios.

## Cómo:
```Clojure
(def archivo (slurp "ruta/al/archivo.txt"))
```
Este código utiliza la función *slurp* para leer el archivo de texto y almacenar su contenido en la variable *archivo*. Podemos imprimir el contenido del archivo utilizando la función *println*:
```Clojure
(println archivo)
```
La salida será el contenido del archivo de texto. Podemos también especificar la codificación del archivo utilizando el parámetro opcional de *slurp*:
```Clojure
(def archivo (slurp "ruta/al/archivo.txt" :encoding "UTF-8"))
```

## Profundizando:
En la historia de Clojure, la función *slurp* fue añadida en la versión 1.2 para simplificar la lectura de archivos de texto. Sin embargo, también podemos utilizar la función *reader* para obtener un objeto que nos permite leer el archivo línea por línea. Otras alternativas para leer archivos de texto en Clojure incluyen la librería [clojure.data.csv](https://github.com/clojure/data.csv) para archivos CSV y [clojure.data.json](https://github.com/clojure/data.json) para archivos JSON.

## Ver también:
- Documentación oficial de Clojure sobre [slurp](https://clojure.org/api/api#clojure.core/slurp)
- Tutorial de Clojure sobre [lectura de archivos](https://clojure.org/guides/io#reading_text_files)