---
title:                "Clojure: Creando un archivo temporal"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# ¿Por qué crear un archivo temporal en Clojure?

La creación de archivos temporales es una práctica común para almacenar información temporalmente durante la ejecución de un programa. En Clojure, esto puede ser especialmente útil para manipular datos y realizar tareas específicas en un entorno controlado antes de guardarlos permanentemente.

## Cómo hacerlo

Para crear un archivo temporal en Clojure, utilizamos la función `with-open` para abrir el archivo y escribir en él utilizando `spit`. A continuación se muestra un ejemplo de código que crea un archivo temporal y escribe el texto "¡Hola Mundo!" en él:

```Clojure
(defn crear-archivo-temporal []
  (with-open [temp-file (java.io.File/createTempFile "temp-" ".txt")]
    (spit temp-file "¡Hola Mundo!")))
```

Este código creará un archivo temporal con el nombre "temp-######.txt", donde los # representan un número aleatorio. También podemos especificar una ubicación específica para el archivo temporal utilizando el parámetro `:dir`. Por ejemplo:

```Clojure
(defn crear-archivo-temporal-ubicacion []
  (with-open [temp-file (java.io.File/createTempFile "temp-" ".txt" {:dir "ruta/a/directorio"})]
    (spit temp-file "¡Hola Mundo!")))
```

Una vez que hayamos terminado de trabajar con el archivo temporal, podemos cerrarlo utilizando la función `close`:

```Clojure
(defn cerrar-archivo []
  (with-open [temp-file (java.io.File/createTempFile "temp-" ".txt")]
    (spit temp-file "¡Hola Mundo!")
    (close temp-file)))
```

## Profundizando

La función `with-open` utiliza un bloque de código llamado "forma" para abrir y cerrar automáticamente el archivo, lo que garantiza que el archivo sea cerrado correctamente incluso en caso de excepciones. También podemos utilizar la función `deleteOnExit` para eliminar el archivo temporal una vez que se cierre.

```Clojure
(with-open [temp-file (java.io.File/createTempFile "temp-" ".txt")]
  (.deleteOnExit temp-file)
  (spit temp-file "¡Hola Mundo!"))
```

Además, existen otras opciones para crear archivos temporales en Clojure, como la función `java.io.File/createTempFile` que permite crear un archivo en una ubicación específica, o la librería `temp-files` que proporciona funciones específicas para crear y manipular archivos temporales.

## Ver también

- [Documentación oficial de Clojure sobre creación de archivos temporales](https://clojure.org/reference/java_interop#_temporary_files)
- [Ejemplos de archivo temporal con Clojure](https://stackoverflow.com/questions/46063614/create-a-temporary-file-in-clojure) 
- [Librería temp-files para manipular archivos temporales en Clojure](https://github.com/pjstadig/temp-files)