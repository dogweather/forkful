---
date: 2024-01-26 04:12:44.751038-07:00
description: "C\xF3mo: El REPL es clave en la filosof\xEDa de desarrollo interactivo\
  \ de la familia Lisp, y Clojure, un dialecto moderno de Lisp, hace un gran uso de\
  \ esta\u2026"
lastmod: '2024-04-05T22:51:12.450151-06:00'
model: gpt-4-0125-preview
summary: "El REPL es clave en la filosof\xEDa de desarrollo interactivo de la familia\
  \ Lisp, y Clojure, un dialecto moderno de Lisp, hace un gran uso de esta herramienta."
title: Usando una shell interactiva (REPL)
weight: 34
---

## Cómo:
Comienza lanzando REPL:

```Clojure
user=> (println "¡Hola, REPL!")
¡Hola, REPL!
nil
```

Define una función y pruébala:
```Clojure
user=> (defn greet [name] (str "Hola, " name "!"))
#'user/greet
user=> (greet "Programador de Clojure")
"Hola, Programador de Clojure!"
```

Experimenta con estructuras de datos:
```Clojure
user=> (def my-map {:a 1 :b 2})
#'user/my-map
user=> (assoc my-map :c 3)
{:a 1, :b 2, :c 3}
```

## Profundización
El REPL es clave en la filosofía de desarrollo interactivo de la familia Lisp, y Clojure, un dialecto moderno de Lisp, hace un gran uso de esta herramienta. Data desde el primer REPL de Lisp a finales de los años 50. Alternativas en otros lenguajes incluyen el intérprete de Python y la consola de Node.js, pero el REPL de Clojure tiene un estatus de primera clase y es integral al flujo de trabajo.

Una sesión de REPL en Clojure se puede integrar en varios entornos como la línea de comandos, IDEs (tal como IntelliJ con Cursive, o Emacs con CIDER), o herramientas basadas en navegador como Nightcode. En un sentido más profundo, el REPL empodera al desarrollador para manipular las construcciones del lenguaje en tiempo de ejecución y llevar estados a través de varias transformaciones, lo que a menudo lleva a la programación exploratoria y un código más robusto.

La funcionalidad del REPL brilla con herramientas como `lein repl` o `clj`, que permiten la gestión de dependencias, varios plugins y personalizaciones específicas del proyecto, llevando a un proceso de desarrollo más productivo y flexible.

## Ver También
- La guía oficial del sitio web de Clojure sobre el REPL: https://clojure.org/guides/repl/introduction
- La charla de Rich Hickey sobre el desarrollo impulsado por REPL: https://www.youtube.com/watch?v=Qx0-pViyIDU
- Clojure práctico: usar el REPL para desarrollo iterativo: http://practicalclj.blogspot.com/2009/10/using-clojure-repl.html
