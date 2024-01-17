---
title:                "Comprobando si existe un directorio"
html_title:           "Clojure: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Comprobar si un directorio existe es una forma de verificar si una determinada carpeta o directorio está presente en una ruta de archivos. Los programadores a menudo utilizan esta función para asegurar que un directorio necesario para la ejecución del código esté disponible antes de continuar con la ejecución del programa.

## Cómo hacerlo:

```Clojure
(require '[clojure.java.io :refer [file]])

;; Verificar si el directorio "mi_carpeta" existe en la ruta "home/usuario"
(println (file-exists? "home/usuario/mi_carpeta"))

;; Output: false

;; Crear el directorio "mi_carpeta"
(java.io.File. "home/usuario/mi_carpeta").mkdirs()

;; Verificar si el directorio ahora existe
(println (file-exists? "home/usuario/mi_carpeta"))

;; Output: true

```

## Inmersión profunda:

En versiones anteriores de Clojure (antes de la versión 1.7), la función utilizada para verificar si un directorio existe era ```file-seq```. Sin embargo, esta función ha sido reemplazada por ```file-exists?``` en las versiones más recientes. Alternativamente, se puede utilizar la librería ```java.nio.file``` para realizar la misma acción.

## Ver también:

- Documentación oficial de Clojure sobre ```file-exists?```: https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/file-exists%3F
- Ejemplos de uso de ```file-exists?``` en la comunidad Clojure: https://stackoverflow.com/questions/24126259/ensure-a-directory-exists-in-clojure
- Más información sobre la librería ```java.nio.file``` : https://docs.oracle.com/javase/tutorial/essential/io/fileio.html