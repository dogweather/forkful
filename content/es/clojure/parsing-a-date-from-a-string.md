---
title:                "Analizando una fecha de una cadena."
html_title:           "Clojure: Analizando una fecha de una cadena."
simple_title:         "Analizando una fecha de una cadena."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Parsing o análisis de fechas a partir de una cadena es una práctica común en la programación para convertir una cadena de texto en un objeto de fecha y hora en un formato legible. Los programadores hacen esto para realizar cálculos basados en fechas, validar entradas de usuario y mostrar información en un formato más fácil de entender.

## How to:

El siguiente es un ejemplo simple de cómo analizar una fecha a partir de una cadena en Clojure:

```
;; Importar la librería necesaria
(require '[java-time :as t])

;; Crear una cadena con la fecha
(def fecha "2021-08-02")

;; Analizar la fecha utilizando el método parse de la clase LocalDate
(def fecha-analizada (t/parse fecha "yyyy-MM-dd"))

;; Imprimir la fecha en un formato legible
(println fecha-analizada)

;; Salida
#object[java.time.LocalDate 0x4263d6ba "2021-08-02"]
```

## Deep Dive:

Parsing de fechas a partir de una cadena ha sido una tarea desafiante para los programadores durante mucho tiempo. Antes del surgimiento de las librerías de fecha y hora en lenguajes de programación, los desarrolladores tenían que escribir código tedioso y propenso a errores para analizar fechas. Alternativamente, podían utilizar librerías de terceros, pero esto requería una curva de aprendizaje adicional y aumentaba la complejidad del código.

Clojure, basado en el lenguaje de programación funcional Lisp, facilita el análisis de fechas a partir de una cadena mediante la inclusión de la librería de fecha y hora de Java. Esto permite a los programadores aprovechar la infraestructura y funcionalidades de Java sin tener que escribir un código adicional.

## See Also:

- [Documentación oficial sobre Clojure date/time](https://clojure.org/reference/java_interop#date_time)
- [Librería de fecha y hora de Java](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)