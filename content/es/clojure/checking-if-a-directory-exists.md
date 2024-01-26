---
title:                "Comprobando si existe un directorio"
html_title:           "Bash: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Comprobar si un directorio existe implica verificar que una ruta de acceso específica corresponde a un directorio en el sistema de archivos. Los programadores realizan esta comprobación para evitar errores como intentar leer un directorio que no existe o crear duplicados.

## Cómo hacerlo:

```Clojure
; Importamos el namespace para operaciones de archivos
(require '[clojure.java.io :as io])

; Función para verificar si un directorio existe
(defn directory-exists? [path]
  (let [file (io/file path)]
    (and (.exists file) (.isDirectory file))))

; Ejemplo de uso
(println "¿Existe el directorio '/usr/bin'? " (directory-exists? "/usr/bin"))
(println "¿Existe el directorio '/path/to/nonexistent/dir'? " (directory-exists? "/path/to/nonexistent/dir"))
```

**Salida de ejemplo:**
```
¿Existe el directorio '/usr/bin'? true
¿Existe el directorio '/path/to/nonexistent/dir'? false
```

## Profundizando:

Históricamente, la necesidad de verificar la existencia de directorios viene del hecho de que las operaciones con archivos incorrectos pueden causar errores críticos e interrumpir el flujo de un programa. En Clojure, la interacción con el sistema de archivos se gestiona a menudo a través de extensiones del Java IO, que provee un alto nivel de interoperabilidad.

Alternativas para la verificación incluyen el uso de funciones en bibliotecas de terceros o incluso llamadas directas al sistema operativo, pero esto último puede resultar en código menos portátil.

La implementación mostrada usa Java interop (`clojure.java.io/file`) para acceder a métodos de la clase `File` de Java que determinan si una ruta de acceso corresponde a un directorio existente (`exists`) y si esa ruta es realmente un directorio (`isDirectory`).

## Ver También:

- Documentación de Clojure's `clojure.java.io`: [clojure.github.io/clojure/clojure.java.io-api.html](https://clojure.github.io/clojure/clojure.java.io-api.html)
- Guía de interoperabilidad Java y Clojure: [clojure.org/reference/java_interop](https://clojure.org/reference/java_interop)
