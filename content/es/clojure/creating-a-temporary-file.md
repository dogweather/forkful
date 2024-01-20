---
title:                "Creando un archivo temporal"
html_title:           "Arduino: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Crear un archivo temporal es esencialmente la generación de un archivo de corta duración que se utiliza para almacenar información temporalmente durante la ejecución de un programa. Los programadores lo hacen para agilizar el procesamiento de datos evitando operaciones costosas de tiempo y espacio.

## Cómo hacerlo:
En Clojure, puedes hacerlo utilizando las bibliotecas `java.nio.file`. Vamos a ver cómo.

``` Clojure
(import '[java.nio.file Files])
(defn create-temp []
(let [path (Files/createTempFile "prefijo" ".sufijo")]
(.toFile path)))
```
Nuestro archivo temporal se crea con un prefijo y sufijo especificado. Ahora veamos cómo obtener la salida de nuestro archivo temporal.

``` Clojure
(defn read-temp [temp-file]
(let [reader (clojure.java.io/reader temp-file)]
(doseq [line (line-seq reader)]
(println line))))
```
Esto leerá nuestro archivo temporal y mostrará la salida.

## Inmersión Profunda:
Históricamente, la creación y utilización de archivos temporales ha sido una estrategia común para manejar grandes cantidades de datos, especialmente en el procesamiento batch y otras operaciones de computación pesada.

Un enfoque alternativo para el manejo de grandes conjuntos de datos es el uso de bases de datos en memoria, aunque estas pueden ser costosas en términos de espacio y pueden no ser adecuadas para todos los casos de uso.

En cuanto a los detalles de implementación, `java.nio.file` proporciona un manejo más granular de los archivos y permite especificar opciones como la ubicación del archivo temporal y los atributos del archivo.

## Ver También:
1. Documentación de java.nio.file.Files: https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html
2. Clojure Java Interoperability: https://clojure.org/reference/java_interop
3. Manejo de archivos en Clojure: https://www.baeldung.com/clojure-file-io