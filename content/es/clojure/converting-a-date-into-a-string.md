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

## ¿Qué y por qué?
Convertir una fecha en una cadena de texto significa tomar una fecha en formato de datos y convertirla a un formato de texto legible para los humanos. Los programadores hacen esto para mostrar fechas en un formato específico o para trabajar con ellas en programas que solo aceptan cadenas de texto.

## Cómo:
Aquí hay dos ejemplos que muestran cómo convertir una fecha en una cadena de texto usando Clojure:
```Clojure
(require '[clj-time.coerce :as ct])

(-> (local-date 2021 10 15)
    ct/to-string)
```
Este ejemplo usa la biblioteca "clj-time" de Clojure para convertir una fecha en el formato de texto predeterminado (ISO-8601). Si queremos especificar un formato diferente, podemos utilizar la función "format" en su lugar:
```Clojure
(require '[clj-time.format :as fmt])

(-> (local-date 2021 10 15)
    (fmt/format "dd/MM/yyyy"))
```
Este segundo ejemplo utiliza la misma biblioteca, pero especifica un formato diferente para la fecha. En este caso, estamos usando el formato día/mes/año.

## Profundizando:
Conversión de una fecha en una cadena de texto es un proceso común en la programación, ya que permite a los programadores mostrar fechas en un formato específico que es más fácil de entender para los usuarios finales. Aunque hay varias bibliotecas y funciones disponibles en diferentes lenguajes de programación para realizar esta tarea, la biblioteca "clj-time" de Clojure es una de las opciones más populares.

Otra alternativa es usar la biblioteca "java.time" (introducida en Java 8), que contiene métodos específicos para convertir fechas en cadenas de texto en Java. Sin embargo, si estás trabajando en un proyecto que ya utiliza Clojure, puede ser más conveniente seguir utilizando la biblioteca "clj-time" en lugar de importar una biblioteca externa.

Para aquellos que estén interesados en cómo se implementa la conversión de una fecha en una cadena de texto en Clojure, la biblioteca "clj-time" en realidad utiliza la biblioteca "joda-time" de Java por debajo. Joda-Time es una conocida biblioteca de Java que se utiliza comúnmente para trabajar con fechas y tiempos.

## Ver también:
- Clj-time: https://github.com/clj-time/clj-time
- Java.time: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- Joda-Time: https://www.joda.org/joda-time/