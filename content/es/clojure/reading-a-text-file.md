---
title:                "Lectura de un archivo de texto"
date:                  2024-01-20T17:53:58.415796-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lectura de un archivo de texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Leer un archivo de texto es obtener su contenido para procesarlo. Los programadores hacen esto para manipular datos, configuraciones o simplemente importar información.

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
