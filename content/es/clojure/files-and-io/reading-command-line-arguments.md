---
date: 2024-01-20 17:55:35.356802-07:00
description: "C\xF3mo Hacerlo: En Clojure, accedes a los argumentos de la l\xEDnea\
  \ de comandos con `*command-line-args*`. Aqu\xED hay un ejemplo simple."
lastmod: '2024-03-13T22:44:58.673185-06:00'
model: gpt-4-1106-preview
summary: "En Clojure, accedes a los argumentos de la l\xEDnea de comandos con `*command-line-args*`."
title: "Lectura de argumentos de l\xEDnea de comandos"
weight: 23
---

## Cómo Hacerlo:
En Clojure, accedes a los argumentos de la línea de comandos con `*command-line-args*`. Aquí hay un ejemplo simple:

```Clojure
(defn -main [& args]
  (println "Argumentos recibidos:" args))

;; Si ejecutas el programa así:
;; clojure -M your_script.clj uno dos tres
;; La salida será:
;; Argumentos recibidos: (uno dos tres)
```

## Análisis Profundo:
En Clojure, `*command-line-args*` es una var especial que contiene una lista de argumentos de línea de comandos como strings. Esta funcionalidad viene integrada desde las primeras versiones de Clojure.

Hay otras maneras de manejar argumentos y configuraciones, como usar librerías de terceros que ofrecen más opciones y un parsing más detallado, por ejemplo `tools.cli`.

En cuanto a la implementación, `*command-line-args*` se inicializa antes de que se ejecute la función `-main`, directo del entorno JVM que ejecuta Clojure. Al ser una lista, puedes manipularla con cualquier función de Clojure que trabaje sobre secuencias, como `map`, `reduce`, `filter`, etc.

## Ver También:
- Documentación oficial de Clojure sobre [`*command-line-args*`](https://clojure.org/reference/vars#_command_line_args)
- [`tools.cli`](https://github.com/clojure/tools.cli) para parsing de línea de comandos más avanzado.
