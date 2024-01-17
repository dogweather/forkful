---
title:                "Obteniendo la fecha actual"
html_title:           "Clojure: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Obtener la fecha actual es el acto de obtener la fecha y hora actuales en el sistema en el que estamos trabajando. Los programadores lo hacen para registrar y almacenar la fecha y hora en que ocurre un evento en el sistema, lo que puede ser útil para la auditoría y el seguimiento de cambios.

## Cómo hacerlo:
```
(import [java.util Date])

(def fecha (Date.))

En la primera línea importamos la clase Date de Java para poder acceder a sus métodos. En la segunda línea, utilizamos la función "def" para asignar el valor actual de la fecha y hora a la variable "fecha".
```

## Profundizando:
Obtener la fecha actual puede ser útil en muchas situaciones de programación, desde la gestión de horarios hasta la sincronización de eventos en el sistema. Antes de la popularidad de Java, se utilizaban otros métodos para obtener la fecha actual, como el uso de la función de sistema "time" en UNIX. Sin embargo, con el tiempo, Java se ha convertido en una opción más conveniente y común para los programadores.

## Ver también:
- Documentación de la clase Date de Java: https://docs.oracle.com/javase/6/docs/api/java/util/Date.html
- Alternativa para obtener la fecha actual en Clojure: https://clojuredocs.org/clojure.java-time/local-date-now