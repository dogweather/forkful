---
title:                "Obteniendo la fecha actual"
date:                  2024-01-20T15:13:39.121420-07:00
html_title:           "Bash: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"

category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Obtener la fecha actual en Clojure es simplemente acceder al momento presente en tu programa. Los programadores hacen esto para registrar eventos, controlar el flujo de datos o manejar tareas programadas.

## Cómo hacerlo:
Clojure usa la biblioteca `java.util.Date` para trabajar con fechas. Aquí está cómo puedes obtener la fecha actual:

```Clojure
(import java.util.Date)

(defn current-date []
  (str (Date.)))

(println (current-date))
```

Salida de muestra:
```
"Tue Mar 14 21:02:33 CET 2023"
```

Para obtener una representación más detallada o modificada, podés usar `clj-time`, una biblioteca inspirada por Joda-Time pero más idiomática para Clojure:

```Clojure
(require '[clj-time.core :as time]
         '[clj-time.format :as format])

(defn formatted-current-date []
  (format/unparse (format/formatters :basic-date-time) (time/now)))

(println (formatted-current-date))
```

Salida de muestra:
```
"20230314T202233.000Z"
```

## Profundización
Históricamente, manejar fechas en programación ha sido complejo debido a zonas horarias, formatos y cálculos de fecha. En Java, `java.util.Date` y `java.util.Calendar` han sido reemplazados por clases de la API `java.time` en Java 8 debido a sus limitaciones y complicaciones de uso. Clojure, corriendo en la JVM, puede usar `java.time` directamente, pero bibliotecas como `clj-time` simplifican el proceso ofreciendo una abstracción más adecuada para trabajar con fechas y tiempos de manera funcional.

Alternativas para obtener la fecha incluyen usar las clases `java.time.LocalDateTime` o `java.time.ZonedDateTime` para más precisión en el manejo de zonas horarias. La implementación depende del nivel de detalle y las operaciones que necesites hacer con la fecha.

## Ver También
- [clj-time GitHub Repository](https://github.com/clj-time/clj-time)
- [Clojure documentation on java interop](https://clojure.org/reference/java_interop)
- [Java 8 java.time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
