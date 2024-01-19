---
title:                "Analizando una fecha a partir de una cadena de texto"
html_title:           "Bash: Analizando una fecha a partir de una cadena de texto"
simple_title:         "Analizando una fecha a partir de una cadena de texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

La conversión de una fecha a partir de una cadena (parsing) es un proceso que transforma texto en una representación estructurada de una fecha. Los programadores hacen esto para manipular fechas y trabajar con ellas en formatos más utilizables.

## Cómo hacerlo:

En Clojure, utilizamos la biblioteca `clojure.java-time` para convertir una cadena a una fecha. Aquí está un ejemplo:

```Clojure
(require '[java-time :as jt])

(defn parse-date [date-str]
  (jt/local-date date-str))
```

Si ejecutamos nuestro código con una cadena de fecha, por ejemplo `parse-date "2021-12-01"`, obtendremos como salida `2021-12-01`.

```Clojure
(parse-date "2021-12-01")
; => #object[java.time.LocalDate  "2021-12-01"]
```
## Profundización:

Históricamente, Clojure ha dependido de las bibliotecas de Java para trabajar con fechas y tiempo, debido a que Clojure se ejecuta en la Máquina Virtual de Java (JVM).

Hay alternativas a `java-time`, como `clj-time`, pero `java-time` es actualmente la opción recomendada porque está basada en la API de tiempo de Java 8, que es más moderna y completa.

A nivel de implementación, cuando llamamos a `jt/local-date` con una cadena, se está utilizando la función `LocalDate.parse` de Java para convertir la cadena en un objeto `LocalDate`.

## Vea También:

Puedes explorar más sobre fechas y tiempos en Clojure en los siguientes enlaces:

- Biblioteca java-time de Clojure: https://cljdoc.org/d/java-time/java-time/0.3.2/api/java-time
- API de Java 8 Time: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- Librería clj-time: https://github.com/clj-time/clj-time

Recuerda, trabajar con fechas y cadenas puede ser complicado. Asegúrate de entender bien cómo la conversión de cadena a fecha funciona en tu contexto específico para evitar errores sutiles y difíciles de detectar.