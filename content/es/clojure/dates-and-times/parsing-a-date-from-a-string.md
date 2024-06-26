---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:43.866215-07:00
description: "C\xF3mo hacerlo: Dado que Clojure es un lenguaje JVM, te permite usar\
  \ directamente las bibliotecas de fecha y hora de Java. Comencemos con la\u2026"
lastmod: '2024-03-13T22:44:58.666684-06:00'
model: gpt-4-0125-preview
summary: Dado que Clojure es un lenguaje JVM, te permite usar directamente las bibliotecas
  de fecha y hora de Java.
title: Analizando una fecha a partir de una cadena de texto
weight: 30
---

## Cómo hacerlo:
Dado que Clojure es un lenguaje JVM, te permite usar directamente las bibliotecas de fecha y hora de Java. Comencemos con la interoperabilidad con Java integrada y luego exploremos cómo utilizar una biblioteca de terceros popular, clj-time, para soluciones más idiomáticas de Clojure.

### Usando Interoperabilidad con Java
Clojure puede aprovechar directamente `java.time.LocalDate` de Java para analizar fechas de cadenas:
```clojure
(require '[clojure.java.io :as io])

; Analizando una fecha usando interoperabilidad con Java
(let [date-str "2023-04-01"
      date (java.time.LocalDate/parse date-str)]
  (println date))
; Salida: 2023-04-01
```

### Usando clj-time
Una biblioteca de Clojure más idiomática para tratar con fechas y horas es `clj-time`. Envuelve a Joda-Time, una biblioteca comprensiva para operaciones de fecha y hora. Primero, necesitarás agregar `clj-time` a las dependencias de tu proyecto. Así es como puedes analizar una cadena de fecha usando `clj-time`:

```clojure
; Asegúrate de agregar [clj-time "0.15.2"] a tu project.clj bajo :dependencies

(require '[clj-time.format :as fmt]
         '[clj-time.core :as time])

; Definir un formateador
(let [formatter (fmt/formatter "yyyy-MM-dd")
      date-str "2023-04-01"
      parsed-date (fmt/parse formatter date-str)]
  (println parsed-date))
; Salida: #object[org.joda.time.DateTime 0x76eccb5d "2023-04-01T00:00:00.000Z"]
```

Estos ejemplos demuestran el análisis básico de fechas. Ambos métodos son útiles, pero `clj-time` puede proporcionar un enfoque más centrado en Clojure con funcionalidades adicionales para requisitos complejos.
