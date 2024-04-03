---
date: 2024-01-20 17:41:46.947632-07:00
description: "C\xF3mo hacerlo: ."
lastmod: '2024-03-13T22:44:58.640901-06:00'
model: gpt-4-1106-preview
summary: .
title: "Eliminando caracteres que coinciden con un patr\xF3n"
weight: 5
---

## Cómo hacerlo:
```Clojure
(defn eliminar-patron [texto patron]
  (clojure.string/replace texto patron ""))

;; Ejemplo de uso:
(println (eliminar-patron "H3llo W0rld!" #"[0-9]"))
(println (eliminar-patron "Clojure es 1 2 3 genial" #"\d"))

;; Salida esperada:
;; "Hllo Wrld!"
;; "Clojure es   genial"
```

## Profundizando
Antes de Clojure, lenguajes como Perl o Python ya manejaban eliminación de caracteres con expresiones regulares. ¿Por qué importa? Porque Clojure no reinventó la rueda, tomó lo bueno y lo integró a su estilo. Alternativas: puedes usar funciones de alto nivel como `filter`, pero las regex son más potentes y concisas para esto. A nivel de implementación, el uso de expresiones regulares en Clojure se apoya en la biblioteca de Java, que es robusta y bien probada.

## Ver También
- Expresiones regulares en Clojure: [Clojure Regex](https://clojure.org/guides/learn/functions#_regex)
- Tutorial de Java Regex, útil para entender el funcionamiento de fondo: [Java Regex Tutorial](https://docs.oracle.com/javase/tutorial/essential/regex/)
