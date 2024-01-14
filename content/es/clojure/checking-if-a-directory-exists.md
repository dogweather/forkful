---
title:    "Clojure: Comprobar si existe un directorio."
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por qué

En la programación, a menudo es necesario verificar si un directorio existe antes de realizar operaciones específicas en él. Esto se debe a que si el directorio no existe, las operaciones pueden fallar o generar resultados no deseados.

## Cómo hacerlo

La forma más sencilla de verificar si un directorio existe en Clojure es utilizando la función "exists?" del namespace "clojure.java.io". Esta función toma una ruta de directorio como argumento y devuelve un valor booleano que indica si existe o no.

```Clojure
(ns directorio
  (:require [clojure.java.io :as io]))

;; Ruta de directorio a verificar
(def ruta-directorio "ruta/directorio")

;; Verificar si el directorio existe
(io/exists? ruta-directorio)

;; Resultado
true
```

## Profundizando

La función "exists?" en realidad llama a la función "file-exists?" del namespace "java.io.File". Esto significa que también podemos utilizar directamente esta función en lugar de la de Clojure.

Otra opción es utilizar la función "dir?" del mismo namespace, que verifica si un directorio existe y si es un directorio válido según el sistema operativo en el que se está ejecutando.

```Clojure
(ns directorio
  (:require [java.io.File]))

;; Ruta de directorio a verificar
(def ruta-directorio "ruta/directorio")

;; Verificar usando file-exists?
(.file-exists (File. ruta-directorio))

;; Verificar usando dir?
(.dir? (File. ruta-directorio))
```

## Ver también

- [Documentación oficial de Clojure sobre la función "exists?"](https://clojuredocs.org/clojure.java.io/exists_q)
- [Documentación oficial de Java sobre la clase "File"](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)
- [Stackoverflow: ¿Cómo puedo comprobar si un directorio existe en Clojure?](https://stackoverflow.com/questions/7019823/how-can-i-check-if-a-directory-exists-in-clojure)