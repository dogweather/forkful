---
title:                "Comprobando si un directorio existe"
aliases:
- /es/clojure/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:02.306717-07:00
model:                 gpt-4-0125-preview
simple_title:         "Comprobando si un directorio existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Verificar si un directorio existe en Clojure implica comprobar la presencia de un directorio en el sistema de archivos desde dentro de tu aplicación Clojure. Esta tarea es crucial para operaciones con archivos, para prevenir errores al leer o escribir en directorios que podrían no estar presentes, asegurando una ejecución de código robusta y libre de errores.

## Cómo hacerlo:
Clojure, siendo un lenguaje JVM, puede utilizar la clase `java.io.File` de Java para este propósito. No necesitas ninguna biblioteca de terceros para una operación tan básica. Así es como puedes hacerlo:

```clojure
(import 'java.io.File)

(defn directory-exists? [dir-path]
  (let [dir (File. dir-path)]
    (.exists dir)))

;; Ejemplo de uso
(println (directory-exists? "/ruta/a/tu/directorio")) ;; verdadero o falso
```

Esta función, `directory-exists?`, toma una ruta de directorio como una cadena y devuelve `true` si el directorio existe y `false` de lo contrario. Esto se logra creando un objeto `File` con la ruta del directorio y luego llamando al método `.exists` en este objeto.

Además de la interoperabilidad directa con Java, puedes usar bibliotecas de Clojure que abstraen parte del código repetitivo de Java. Una de estas bibliotecas es `clojure.java.io`. Sin embargo, para verificar si un directorio existe, todavía usarías la clase `File`, pero podrías encontrar útil la biblioteca para otras operaciones con archivos. Ejemplo:

```clojure
(require '[clojure.java.io :as io])

(defn directory-exists?-clojure [dir-path]
  (.exists (io/file dir-path)))

;; Ejemplo de uso
(println (directory-exists?-clojure "/otra/ruta/para/verificar")) ;; verdadero o falso
```

Esta versión es bastante similar, pero usa la función `io/file` de Clojure para crear el objeto `File`. Este método se integra de manera más natural en bases de código Clojure al aprovechar la biblioteca de Clojure para operaciones IO, en lugar de interactuar directamente con clases de Java.
