---
title:                "Clojure: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Por qué

En la programación, a menudo necesitamos verificar si un directorio existe antes de realizar una acción sobre él. Hacer esta comprobación puede ayudar a evitar errores y a mantener la integridad de nuestro código.

# Cómo hacerlo

Para verificar si un directorio existe en Clojure, podemos utilizar la función `file-seq` junto con `map` y `file?`. Primero, necesitamos importar el módulo `clojure.java.io` para tener acceso a estas funciones.

```Clojure
(ns directorio-existe
  (:require [clojure.java.io :as io]))

;; Directorio que queremos comprobar si existe
(def directorio "/ruta/al/directorio")

;; Verificar si existe
(def existe? (some #(file? %) (map #(io/file %) (file-seq directorio))))

;; Impresión de resultado
(println "¿El directorio" directorio "existe? " existe?)
```

La salida de este código sería `¿El directorio /ruta/al/directorio existe? true`. En caso de que el directorio no exista, el resultado sería `false`.

# Profundizando

La función `file-seq` nos permite obtener una secuencia de archivos y directorios dentro de una ruta determinada. Luego, utilizamos la función `map` para convertir cada elemento de la secuencia en un objeto de tipo `java.io.File`, que luego puede ser utilizado por la función `file?` para verificar si es un directorio.

# Ver también

- Documentación oficial de Clojure sobre [file-seq](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/file-seq)
- Guía de programación sobre [archivos y directorios en Clojure](https://clojure-cookbook.com/files/)