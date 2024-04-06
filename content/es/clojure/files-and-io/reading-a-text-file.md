---
date: 2024-01-20 17:53:58.415796-07:00
description: "C\xF3mo hacerlo: Leer archivos de texto no es nuevo. Desde los d\xED\
  as de Lisp, leer archivos ha sido una operaci\xF3n b\xE1sica. En Clojure, `slurp`\
  \ es una funci\xF3n\u2026"
lastmod: '2024-04-05T21:54:00.030839-06:00'
model: gpt-4-1106-preview
summary: Leer archivos de texto no es nuevo.
title: Lectura de un archivo de texto
weight: 22
---

## Cómo hacerlo:
```Clojure
;; Abre y lee un archivo de texto
(slurp "ruta/al/archivo.txt")

;; Ejemplo con manejo de recursos
(with-open [reader (clojure.java.io/reader "ruta/al/archivo.txt")]
  (doall (line-seq reader)))

;; Output de un ejemplo simple
"Hola, este es el contenido de tu archivo."
```

## Profundización:
Leer archivos de texto no es nuevo. Desde los días de Lisp, leer archivos ha sido una operación básica. En Clojure, `slurp` es una función simple que lee todo el contenido. Sin embargo, para archivos grandes, `line-seq` en conjunto con `with-open` evita cargar todo en memoria, manejando el archivo línea por línea. Aunque `slurp` es fácil, métodos más detallados como `line-seq` te dan control y eficiencia, especialmente útil para archivos grandes.

## Ver También:
- ClojureDocs para `slurp`: https://clojuredocs.org/clojure.core/slurp
- ClojureDocs para `line-seq`: https://clojuredocs.org/clojure.core/line-seq
- Guía de Clojure sobre la entrada/salida: https://clojure.org/guides/deps_and_cli
- Tutorial de Clojure: https://www.braveclojure.com/clojure-for-the-brave-and-true/
