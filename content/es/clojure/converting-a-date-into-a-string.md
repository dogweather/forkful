---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "Clojure: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una fecha en una cadena de texto puede ser útil en diversas situaciones, como por ejemplo, mostrar la fecha actual en un formato legible para el usuario en una interfaz de usuario o guardar la fecha en un archivo de texto.

## Cómo hacerlo

Para convertir una fecha en una cadena de texto en Clojure, podemos utilizar la función `format` del módulo `java.time.format`.

```Clojure
(require '[java.time.format :as time])

;; Convertir la fecha actual en una cadena de texto con el formato "dd/MM/yyyy"
(time/format (java.time.LocalDateTime/now) "dd/MM/yyyy")
```

Esto producirá la salida `"29/10/2021"`, ya que la fecha actual al momento de escribir este artículo es el 29 de octubre de 2021.

Podemos personalizar el formato de la cadena de texto utilizando especificadores de formato. Por ejemplo, si queremos incluir la hora y los minutos, podemos usar "HH:mm" como parte del formato.

```Clojure
;; Convertir la fecha actual en una cadena de texto con el formato "dd/MM/yyyy HH:mm"
(time/format (java.time.LocalDateTime/now) "dd/MM/yyyy HH:mm")
```

Esto producirá la salida `"29/10/2021 12:45"`, ya que en este momento son las 12:45.

Otro ejemplo es si queremos incluir el nombre del mes en lugar de su número, podemos usar "MMMM" como parte del formato.

```Clojure
;; Convertir la fecha actual en una cadena de texto con el formato "dd de MMMM, yyyy"
(time/format (java.time.LocalDateTime/now) "dd 'de' MMMM, yyyy")
```

Esto producirá la salida `"29 de octubre, 2021"`.

## Profundizando

La función `format` utiliza el objeto `DateTimeFormatter` detrás de escena para formatear la fecha en una cadena de texto. Este objeto es altamente configurable y permite especificar patrones personalizados para el formato de la fecha.

Además de `format`, también podemos utilizar otras funciones como `parse` y `ofPattern` del módulo `java.time.format` para convertir una cadena de texto en un objeto `LocalDate` o `LocalDateTime`.

```Clojure
(require '[java.time.format :as time])

;; Convertir una cadena de texto en un objeto LocalDate
(time/parse "23/07/1995" "dd/MM/yyyy")

;; Crear un objeto DateTimeFormatter personalizado
(def custom-formatter (time/ofPattern "d' de 'MMMM, yyyy"))

;; Convertir una cadena de texto con el formato personalizado en un objeto LocalDate
(time/parse "29 de octubre, 2021" custom-formatter)
```

## Ver también

- Documentación oficial de `java.time.format`: https://docs.oracle.com/javase/8/docs/api/java/time/format/package-summary.html
- Tutorial de Clojure sobre manejo de fechas y horas: https://clojure.org/guides/learning_date_time