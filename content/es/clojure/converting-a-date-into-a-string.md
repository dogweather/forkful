---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "C++: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
La conversión de una fecha a una cadena (string) en la programación es el proceso de transformar un objeto de una fecha en una cadena de caracteres. Los programadores lo hacen para manipular, almacenar y presentar datos de fecha de manera más conveniente y coherente.

## Cómo hacerlo:
Clojure ofrece maneras simples para convertir una fecha a una cadena. Con `clj-time` puedes hacerlo de la siguiente manera:

```Clojure
(ns your-namespace
  (:require [clj-time.format :as f]))

(let [fmt (f/formatters :date-time-no-ms)
      date (f/parse fmt "2015-04-01T10:15:30Z")]
  (println (f/unparse fmt date)))
```
La salida será: `"2015-04-01T10:15:30Z"`

## Análisis Profundo
Historialmente, la necesidad de convertir fechas a cadenas proviene de los primeros días de la programación, cuando las fechas se almacenaban como cadenas para ahorrar espacio. Ahora, es principalmente útil para presentar fechas de una manera legible y almacenar fechas en un formato interoperable.

En Clojure hay alternativas como `java.time.format.DateTimeFormatter`. Y también puedes escribir tu propio formateador como este:

```Clojure
(defn format-date [date]
  (let [formatter  (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss'Z'")]
    (.format formatter date)))
```

Respecto a la implementación, Clojure aprovecha la librería Java de manejo de tiempo y fechas, proporcionando sus propias wrappers funcionales sobre estas clases para una mayor comodidad.

## Ver También
2. Documentación oficial de [`clj-time`](https://github.com/clj-time/clj-time)
3. Guía sobre Java Date Time API - [Baeldung Tutorial](https://www.baeldung.com/java-8-date-time-intro)