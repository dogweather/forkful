---
date: 2024-01-20 17:45:25.032817-07:00
description: "Extraer subcadenas significa sacar partes espec\xEDficas de una cadena\
  \ de texto. Los programadores lo hacen para manipular y procesar informaci\xF3n,\
  \ como\u2026"
lastmod: 2024-02-19 22:05:17.236970
model: gpt-4-1106-preview
summary: "Extraer subcadenas significa sacar partes espec\xEDficas de una cadena de\
  \ texto. Los programadores lo hacen para manipular y procesar informaci\xF3n, como\u2026"
title: "Extracci\xF3n de subcadenas"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Extraer subcadenas significa sacar partes específicas de una cadena de texto. Los programadores lo hacen para manipular y procesar información, como obtener nombres de usuario de correos electrónicos o generar resúmenes.

## Cómo Hacerlo:
```clojure
; Usamos "subs" para extraer una subcadena por índices
(def texto "Clojure mola mucho")
(subs texto 8 13) ; => "mola"

; Si queremos partir una cadena usamos "split"
(require '[clojure.string :as str])
(str/split texto #"\s") ; => ["Clojure" "mola" "mucho"]

; Para obtener un prefijo o sufijo sin conocer los índices exactos
(str/starts-with? texto "Clojure") ; => true
(str/ends-with? texto "mucho") ; => true
```

## Inmersión Profunda:
En Clojure, como en otros lenguajes de la familia Lisp, operar con textos es directo gracias a funciones bien definidas. Antes, en lenguajes como C, extraer subcadenas era más complicado y propenso a errores por la gestión manual de la memoria.

Las alternativas para extraer partes de una cadena no se limitan a `subs` y `split`. Librerías como `clojure.string` ofrecen funciones como `replace`, `upper-case`, `lower-case` que pueden usarse en conjunto para preparar los datos antes de extraerles partes significativas.

La implementación de estas funciones está diseñada para ser inmutable, es decir, cada vez que extraes una subcadena, se crea una nueva cadena en lugar de modificar la original. Esto es parte del diseño general de Clojure para promover un código seguro y fácil de entender.

## Ver También:
- Una guía más detallada en el manejo de cadenas en Clojure: [Clojure - Strings](https://www.tutorialspoint.com/clojure/clojure_strings.htm)
