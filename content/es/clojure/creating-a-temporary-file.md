---
title:    "Clojure: Creando un archivo temporal"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

Muchas veces en programación, es necesario crear un archivo temporal para almacenar datos o realizar operaciones temporales. Estos archivos sirven como una herramienta útil para mantener la organización en nuestro código y asegurar que no se pierdan datos importantes.

## Cómo hacerlo

En Clojure, podemos crear un archivo temporal utilizando la función `with-open` y el macro `temporary-file`. Podemos especificar el nombre del archivo y su extensión, así como su ubicación en el sistema.

```Clojure
(with-open [file (temporary-file "foo" ".txt")]
  (println "Se ha creado el archivo" (.getName file)))
```

Este código creará un archivo temporal llamado "foo.txt" en la ubicación predeterminada del sistema. También podemos especificar una ubicación específica para el archivo temporal utilizando la función `with-open`.

```Clojure
(with-open [file (temporary-file "bar" ".csv" "/Users/John/Desktop")]
  (println "Se ha creado el archivo" (.getAbsolutePath file)))
```

Este código creará un archivo temporal llamado "bar.csv" en el escritorio de John.

## Profundizando

Hay algunas cosas importantes a tener en cuenta al crear un archivo temporal en Clojure. Primero, estos archivos se eliminarán automáticamente cuando el bloque `with-open` en el que se crearon se cierre. Además, podemos acceder al objeto `File` del archivo temporal y utilizarlo para realizar operaciones como escribir o leer datos.

Es importante recordar que estos archivos son solo temporales y no deben ser considerados permanentes. Si necesitamos almacenar datos permanentemente, es mejor utilizar un archivo regular en lugar de un archivo temporal.

## Ver También

- [Documentación de Clojure sobre la función `temporary-file`](https://clojuredocs.org/clojure.java.io/temporary-file)
- [Ejemplo de creación de archivo temporal en Clojure](https://gist.github.com/jaseemabid/d5f629ddc1044532ff7865f55ebc692c)