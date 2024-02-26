---
date: 2024-01-20 17:28:40.944773-07:00
description: "Calcular una fecha en el futuro o pasado es b\xE1sicamente sumar o restar\
  \ d\xEDas a una fecha dada. Lo hacemos para programar eventos, calcular vencimientos\
  \ o\u2026"
lastmod: '2024-02-25T18:49:55.231366-07:00'
model: gpt-4-1106-preview
summary: "Calcular una fecha en el futuro o pasado es b\xE1sicamente sumar o restar\
  \ d\xEDas a una fecha dada. Lo hacemos para programar eventos, calcular vencimientos\
  \ o\u2026"
title: "C\xE1lculo de una fecha en el futuro o el pasado"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Calcular una fecha en el futuro o pasado es básicamente sumar o restar días a una fecha dada. Lo hacemos para programar eventos, calcular vencimientos o medir periodos de tiempo.

## How to:
Para trabajar con fechas en Clojure, usaremos la librería `clj-time`, una envoltura para Joda-Time. La instalamos agregando `[clj-time "0.15.2"]` a nuestro archivo `project.clj`. Aquí hay ejemplos simples:

```clojure
(require '[clj-time.core :as t])
(require '[clj-time.coerce :as c])

;; Sumar días a una fecha
(def fecha-actual (t/now))
(def futuro (t/plus fecha-actual (t/days 10)))

;; Restar días a una fecha
(def pasado (t/minus fecha-actual (t/days 10)))

;; Imprimir fechas
(println (c/to-string fecha-actual)) ;; "2023-03-31T18:04:21.236Z"
(println (c/to-string futuro))       ;; "2023-04-10T18:04:21.236Z"
(println (c/to-string pasado))       ;; "2023-03-21T18:04:21.236Z"
```

## Deep Dive:
Calcular fechas es vital desde que los sistemas informáticos empezaron a manejar eventos y tareas programados. Antes teníamos que hacerlo manualmente, pero con evolución de las bibliotecas de manejo de tiempo como Joda-Time y `java.time`, es mucho más sencillo.

Alternativas a `clj-time` incluyen trabajar directamente con `java.time` en Clojure, o bibliotecas como `tick` y `time-literals`. Cada una tiene sus ventajas, pero `clj-time` es muy popular por su capacidad de trabajar con la familiar Joda-Time.

Al calcular fechas en el pasado y futuro, debemos tener en cuenta zonas horarias y cambios como el horario de verano. `clj-time` lo maneja elegantemente, mientras que las operaciones manuales requerirían conocimiento detallado de estos aspectos.

## See Also:
- Documentación `clj-time`: https://github.com/clj-time/clj-time
- Guía sobre la API `java.time`: https://clojure.github.io/clojure/cheatsheets/java-time.html
- Repositorio de `tick`: https://github.com/juxt/tick
- Sintaxis `time-literals`: https://github.com/henryw374/time-literals
