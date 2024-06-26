---
date: 2024-01-26 00:50:58.750284-07:00
description: "C\xF3mo hacerlo: Clojure, al igual que sus ancestros Lisp, se basa en\
  \ las excepciones para tratar los errores. Aqu\xED te mostramos c\xF3mo demostrar\
  \ de qu\xE9 est\xE1s\u2026"
lastmod: '2024-03-13T22:44:58.664282-06:00'
model: gpt-4-1106-preview
summary: Clojure, al igual que sus ancestros Lisp, se basa en las excepciones para
  tratar los errores.
title: Manejo de errores
weight: 16
---

## Cómo hacerlo:
Clojure, al igual que sus ancestros Lisp, se basa en las excepciones para tratar los errores. Aquí te mostramos cómo demostrar de qué estás hecho cuando las cosas se ponen difíciles.

Lanzar una excepción es sencillo:
```Clojure
(throw (Exception. "¡Ups! Algo salió mal."))
```

Capturar una excepción, estarás haciendo esto mucho:
```Clojure
(try
  ;; código riesgoso
  (/ 1 0)
  (catch ArithmeticException e
    (println "¡No se puede dividir por cero!"))
  ;; el bloque finally se ejecuta pase lo que pase
  (finally 
    (println "El código de limpieza va aquí.")))
```
Salida de muestra para el bloque catch de arriba:
```
¡No se puede dividir por cero!
El código de limpieza va aquí.
```

Usando `ex-info` y `ex-data` para un contexto más rico sobre las excepciones:
```Clojure
(try
  ;; provocando una excepción personalizada
  (throw (ex-info "Error personalizado" {:type :fallo-personalizado}))
  (catch Exception e
    ;; obteniendo los datos de nuestra excepción personalizada
    (println (ex-data e))))
```
Salida de muestra:
```
{:type :fallo-personalizado}
```

## Inmersión Profunda
La historia del manejo de errores en Clojure no es radicalmente diferente de otros Lisps o incluso de Java (de donde hereda el mecanismo `try-catch`). Es pragmático; usar excepciones es el camino principal, al igual que Java, pero Clojure ofrece un sabor funcional con `ex-info` y `ex-data` para datos de error más enriquecidos.

Las alternativas para el manejo de errores en Clojure incluyen el uso de construcciones monádicas, como la monada `either` de bibliotecas como `cats`, o core.async para la propagación de errores basada en canales. Sin embargo, estas son más complejas y utilizadas en escenarios específicos.

Históricamente, el manejo de errores en los lenguajes de programación ha evolucionado desde simples retornos de estado hasta los mecanismos de manejo de excepciones más sofisticados de los lenguajes modernos. Clojure opta por la simplicidad y un toque de programación funcional, fusionando lo antiguo con lo nuevo.

## Ver También
- Guía de Clojure para las excepciones: https://clojure.org/guides/exceptions
- Biblioteca “Cats” para enfoques más funcionales: https://github.com/funcool/cats
- “Core.async” para programación asincrónica: https://github.com/clojure/core.async
