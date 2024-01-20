---
title:                "Verificando si un directorio existe"
html_title:           "Clojure: Verificando si un directorio existe"
simple_title:         "Verificando si un directorio existe"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Comprobar si un directorio existe es una tarea esencial en programación para prevenir errores y garantizar el flujo correcto del programa. Nos ayuda a evitar la creación de archivos o directorios duplicados o la lectura fallida de un directorio inexistente.

## Cómo hacerlo:

En Clojure, la biblioteca java.nio.file.Files se usa para verificar si un directorio existe. Aquí tienes código ejemplo:

```clojure
(import 'java.nio.file.Files 'java.nio.file.Paths)

(defn directory-exists? [dir-path]
  (Files/exists (Paths/get dir-path (into-array String []))))
```

Uso de código:

```clojure
(directory-exists? "/home/user/Documents")
```

En caso de que el directorio exista, el código devolverá `true`; si no, será `false`.

## Inmersión profunda:

En los primeros días de Java, se usaba la clase 'File' para verificar la existencia de directorios. Sin embargo, java.nio.file, introducido en Java 7, es más versátil y eficiente.

Una alternativa a `Files/exists` es la función `Files/notExists`. Esta devolverá `true` si el directorio no existe, y `false` en caso contrario.

En la implementación real, `Files/exists` usa la función nativa `stat` en sistemas Unix e `GetFileAttributesEx` en Windows para verificar la existencia del directorio.

## Ver también:

- Documentación oficial de Oracle sobre `java.nio.file`: 
[Oracle’s java.nio.file documentation](https://docs.oracle.com/javase/8/docs/api/java/nio/file/package-summary.html)
- Guía de inicio rápido de Clojure para Java Devs: 
[Getting Started with Clojure for Java Developers](https://clojure.org/guides/getting_started)