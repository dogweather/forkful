---
date: 2024-01-20 17:39:54.162274-07:00
description: "How to: La funci\xF3n `spit` escribe el contenido al archivo, `slurp`\
  \ lo lee."
lastmod: '2024-04-05T21:54:00.033049-06:00'
model: gpt-4-1106-preview
summary: "La funci\xF3n `spit` escribe el contenido al archivo, `slurp` lo lee."
title: Creando un archivo temporal
weight: 21
---

## How to:
```Clojure
; Importar la biblioteca para trabajar con archivos
(require '[clojure.java.io :as io])

; Crear un archivo temporal y escribir algo en él
(let [temp-file (io/file (io/temp-dir) "my-temp-prefix.txt")]
  (spit temp-file "Contenido temporal")
  (slurp temp-file))  ; => "Contenido temporal"
```
La función `spit` escribe el contenido al archivo, `slurp` lo lee.

## Deep Dive
En el pasado, la gestión de archivos temporales era más manual y propensa a errores, como olvidar borrar el archivo después de usarlo. Clojure, aprovechando la JVM, simplifica este proceso. Alternativas para la creación de archivos temporales incluyen el uso de bibliotecas de terceros o manejo de archivos directamente con Java NIO. A nivel de implementación, `io/temp-dir` obtiene el directorio temporal del sistema que puede variar según la plataforma (por ejemplo, `/tmp` en UNIX), y `io/file` crea una instancia java.io.File que representa al archivo temporal.

## Ver También
- ClojureDocs para un repaso más profundo en el manejo de archivos: https://clojuredocs.org/clojure.java.io
- Documentación de la clase `java.io.File`: https://docs.oracle.com/javase/7/docs/api/java/io/File.html
- Guía sobre java.nio.file para manejo avanzado de archivos en Clojure: https://docs.oracle.com/javase/tutorial/essential/io/fileio.html
