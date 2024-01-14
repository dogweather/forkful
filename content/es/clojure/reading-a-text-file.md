---
title:                "Clojure: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por Qué

Leer un archivo de texto puede ser una tarea sencilla, pero hay muchas razones por las que un programador en Clojure podría querer hacerlo. Puede ser necesario procesar datos almacenados en un archivo de texto, realizar análisis de texto o simplemente leer un archivo para obtener información.

## Cómo Hacerlo

Para leer un archivo de texto en Clojure, primero necesitas crear una instancia de `java.io.BufferedReader` utilizando la función `clojure.java.io/reader`. Esta función toma como argumento una cadena que representa la ruta del archivo. Por ejemplo:

```Clojure
(def file (clojure.java.io/reader "ruta/al/archivo.txt"))
```

Una vez que tengas una instancia de `java.io.BufferedReader`, puedes leer el contenido del archivo utilizando la función `read-line`. Esta función lee una línea del archivo y devuelve una cadena con el contenido. Puedes usar un bucle para leer todas las líneas del archivo, por ejemplo:

```Clojure
(loop [line (.readLine file)]
  (when line
    (println line)
    (recur (.readLine file))))
```

Esta muestra es solo un ejemplo, asegúrate de cerrar la instancia de `java.io.BufferedReader` luego de leer el archivo para liberar recursos.

## Profundizando

Además de la función `read-line`, también puedes utilizar la función `slurp` para leer todo el contenido de un archivo en una sola cadena. Esta función puede tomar un argumento opcional para especificar un formato de codificación si es necesario.

Otra opción interesante es la función `line-seq` que te permite leer todas las líneas de un archivo y devolver una secuencia de líneas. Esto puede ser útil si necesitas trabajar con el archivo en formato de secuencia en lugar de una sola cadena.

Cabe mencionar que las funciones mencionadas anteriormente son solo algunas de las opciones disponibles en Clojure para leer archivos de texto. Si quieres profundizar más en el tema, te invitamos a consultar la documentación oficial de Clojure.

## Ver También

- [Documentación oficial de Clojure sobre la función `clojure.java.io/reader`] (https://clojuredocs.org/clojure.java.io/reader)
- [Documentación oficial de Clojure sobre la función `read-line`] (https://clojuredocs.org/clojure.core/read-line)
- [Documentación oficial de Clojure sobre la función `slurp`] (https://clojuredocs.org/clojure.core/slurp)
- [Documentación oficial de Clojure sobre la función `line-seq`] (https://clojuredocs.org/clojure.core/line-seq)