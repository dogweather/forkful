---
title:                "Clojure: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# ¿Por qué crear un archivo temporal en Clojure?

La creación de archivos temporales es una parte común en el desarrollo de software, ya que nos permite realizar tareas como almacenar datos temporales, realizar pruebas de código y mantener la organización de nuestro proyecto. En Clojure, hay varias formas de crear y trabajar con archivos temporales, lo que nos brinda más flexibilidad en nuestro proceso de programación.

## Cómo crear un archivo temporal en Clojure

Una forma sencilla de crear un archivo temporal en Clojure es utilizando la función `with-open` junto con `io/file` para crear el archivo en el directorio temporal predeterminado y escribir en él. Este archivo se eliminará automáticamente una vez que el programa finalice. Veamos un ejemplo de cómo hacerlo:

```Clojure
(with-open [temp-file (io/file (System/getProperty "java.io.tmpdir") "mi_archivo_temporal.txt")]
  (.write temp-file "Hola, mundo!")
```

Al ejecutar este código, se creará un archivo llamado "mi_archivo_temporal.txt" en la carpeta temporal del sistema y se escribirá el texto "Hola, mundo!" en él.

## Profundizando en la creación de archivos temporales

Si queremos tener más control sobre la creación y eliminación de nuestros archivos temporales en Clojure, podemos utilizar la librería `clojure.java.io` y su función `as-file` para crear un objeto `File` a partir del cual podemos realizar diversas operaciones, como crear un directorio temporal específico o cambiar el nombre del archivo creado. Además, podemos utilizar la función `with-open` con una expresión lambda para asegurarnos de que el archivo temporal se cierre correctamente después de su uso. Veamos un ejemplo:

```Clojure
(require '[clojure.java.io :as io])

(with-open [temp-file (io/as-file "/Users/usuario/archivo_temporal.txt")]
  (.delete temp-file)) ; borra el archivo temporal creado anteriormente

(with-open [temp-file (io/as-file "/Users/usuario/nuevo_directorio/archivo_temporal.txt")]
  (.getParentFile temp-file)) ; devuelve el directorio en el que se encuentra el archivo temporal creado
```

## Consulta también

- Documentación oficial de Clojure sobre la creación de archivos temporales: https://clojure.org/reference/io#temporary-files
- Tutorial sobre el uso de la librería `clojure.java.io`: https://www.braveclojure.com/working-with-files-in-clojure/