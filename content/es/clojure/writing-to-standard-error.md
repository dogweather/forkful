---
title:                "Escribiendo en el error estándar"
html_title:           "Arduino: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qué es y por qué?
Escribir en el error estándar (stderr) es enviar mensajes de error o log de diagnóstico a un canal de salida específico. Programadores lo usan para separar errores de los datos de salida (stdout), ayudando así a depurar y monitorizar programas sin interferir con la salida regular.

## Cómo hacerlo:
Clojure, como lenguaje en la JVM, usa los métodos de Java para escribir en stderr. Aquí hay un ejemplo:

```Clojure
;; Impresión simple en stderr
(. System err (println "¡Ups! Ocurrió un error"))

;; Uso de println directamente desde clojure.core
(clojure.core/binding [*err* *out*]
  (println "Esto también va a stderr"))

;; Escribiendo una excepción en stderr
(try
  (throw (Exception. "Algo salió mal"))
  (catch Exception e
    (. System err (println (.getMessage e)))))
```

Sample Output:
```
¡Ups! Ocurrió un error
Esto también va a stderr
Algo salió mal
```

## Deep Dive:
Escribir en stderr data desde los tiempos de los sistemas Unix y es una convención que la mayoría de los lenguajes de programación siguen. Además de `System/err`, se pueden usar otras bibliotecas en Clojure, como `tools.logging` para manejar logs más avanzados. En el nivel de implementación, Clojure usa la infraestructura de Java para stderr, que se puede redirigir o manipular como cualquier `java.io.PrintStream`.

## See Also:
- Documentación oficial de Clojure: [clojure.org](https://clojure.org/)
- Documentación de Java sobre `System.err`: [System (Java Platform SE)](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
- Librería de logging para Clojure: [tools.logging](https://github.com/clojure/tools.logging)
