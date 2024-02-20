---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:09.916572-07:00
description: "Obtener la fecha actual en programaci\xF3n es crucial por una mir\xED\
  ada de razones, incluyendo el registro (logging), marcar eventos con una fecha y\
  \ hora\u2026"
lastmod: 2024-02-19 22:05:17.258353
model: gpt-4-0125-preview
summary: "Obtener la fecha actual en programaci\xF3n es crucial por una mir\xEDada\
  \ de razones, incluyendo el registro (logging), marcar eventos con una fecha y hora\u2026"
title: Obteniendo la fecha actual
---

{{< edit_this_page >}}

## Qué y Por Qué?
Obtener la fecha actual en programación es crucial por una miríada de razones, incluyendo el registro (logging), marcar eventos con una fecha y hora determinadas, y la programación de tareas. En Clojure, un dialecto de Lisp en la JVM, esta tarea aprovecha las capacidades de interoperabilidad con Java, permitiendo un acceso sencillo a la rica API de Fecha y Hora de Java.

## Cómo hacerlo:

### Usando la Interoperabilidad con Java
La interoperabilidad sin fisuras de Clojure con Java te permite acceder directamente a la API de Fecha y Hora de Java. Así es como puedes obtener la fecha actual:

```clojure
(import java.time.LocalDate)

(defn get-current-date []
  (str (LocalDate/now)))

;; Salida de muestra
(get-current-date) ; "2023-04-15"
```

### Usando la Biblioteca clj-time
Para una solución más idiomática en Clojure, podrías optar por la biblioteca `clj-time`, un envoltorio alrededor de Joda-Time, aunque para la mayoría de los proyectos nuevos, se recomienda la API de Fecha y Hora incorporada en Java 8. Sin embargo, si prefieres o necesitas `clj-time`:

Primero, añade `clj-time` a las dependencias de tu proyecto. En tu `project.clj`, incluye:

```clojure
[clj-time "0.15.2"]
```

Luego, úsala para obtener la fecha actual:

```clojure
(require '[clj-time.core :as time])

(defn get-current-date-clj-time []
  (str (time/now)))

;; Salida de muestra
(get-current-date-clj-time) ; "2023-04-15T12:34:56.789Z"
```

Ambos métodos proporcionan formas rápidas y efectivas de obtener la fecha actual en Clojure, aprovechando el poder de la plataforma Java subyacente o la conveniencia de una biblioteca específica de Clojure.
