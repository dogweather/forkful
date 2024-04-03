---
date: 2024-01-20 17:36:20.718303-07:00
description: "C\xF3mo hacerlo: Clojure, por ser un dialecto de Lisp y correr en la\
  \ JVM, utiliza las clases de Java para manejar fechas. Aqu\xED hay un ejemplo de\
  \ c\xF3mo\u2026"
lastmod: '2024-03-13T22:44:58.668943-06:00'
model: gpt-4-1106-preview
summary: Clojure, por ser un dialecto de Lisp y correr en la JVM, utiliza las clases
  de Java para manejar fechas.
title: Convirtiendo una fecha en una cadena de texto
weight: 28
---

## Cómo hacerlo:
Clojure, por ser un dialecto de Lisp y correr en la JVM, utiliza las clases de Java para manejar fechas. Aquí hay un ejemplo de cómo convertir una fecha a una cadena:

```Clojure
(import java.text.SimpleDateFormat)
(import java.util.Date)

(defn convertir-fecha-a-cadena [fecha]
  (let [formato-fecha "dd-MM-yyyy"]
    (-> (SimpleDateFormat. formato-fecha)
        (.format fecha))))

(println (convertir-fecha-a-cadena (Date.)))
```

Salida de ejemplo:

```
"23-03-2023"
```

## Profundidad:
Históricamente, manejar fechas y conversiones no fue tan directo en Java (y por ende en Clojure). La API de fecha tiempo en Java 8 mejoró la situación, pero Clojure mantiene la compatibilidad con versiones anteriores de Java, por lo que aún se ven estos métodos anticuados en uso.

Alternativas modernas usan la biblioteca `clj-time`, un envoltorio de la API Joda-Time, o la API `java.time` de Java 8. `clj-time` proporciona una interfaz más idiomática y funciones ricas para Clojure, mientras que `java.time` ofrece inmutabilidad y manejo de zonas horarias más sofisticado.

Detalles de implementación incluyen el manejo de zonas horarias y la configuración del idioma.

Ejemplo usando `java.time`:

```Clojure
(import java.time.ZonedDateTime)
(import java.time.format.DateTimeFormatter)

(defn convertir-fecha-a-cadena-java-time [fecha]
  (-> (DateTimeFormatter/ofPattern "dd-MM-yyyy")
      (.format fecha)))

(println (convertir-fecha-a-cadena-java-time (ZonedDateTime/now)))
```

## Ver También:
- Documentación oficial de Clojure sobre fechas y tiempo: https://clojure.org/reference/dates_and_times
- `clj-time` en GitHub: https://github.com/clj-time/clj-time
- Tutorial de `java.time`: https://www.baeldung.com/java-8-date-time-intro
