---
title:                "Escribiendo a la salida de error estándar"
html_title:           "Clojure: Escribiendo a la salida de error estándar"
simple_title:         "Escribiendo a la salida de error estándar"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir a la salida de error estándar es una técnica utilizada por los programadores para mostrar mensajes de error o advertencia durante la ejecución de un programa. Esto les permite detectar y solucionar problemas en su código de manera más eficiente.

## Cómo:

```Clojure
(defn divide [x y]
  (when (zero? y)
    (System/err "No se puede dividir por cero"))
  (/ x y))

;; Ejemplo de salida de error
(divide 10 0)
;; No se puede dividir por cero
```

## Profundizando:

La escritura a la salida de error estándar es una función estándar en muchos lenguajes de programación, incluyendo Clojure. Su origen se remonta a los primeros días de la programación cuando los programadores utilizaban un dispositivo llamado "consola" para imprimir mensajes de error.

Aunque escribir a la salida de error es una forma útil de mostrar mensajes de error, es importante mencionar que también existen otras técnicas, como lanzar excepciones o imprimir mensajes en la consola a través de la función "println". Cada una tiene sus ventajas y desventajas, y los programadores pueden elegir la que mejor se adapte a sus necesidades.

La implementación de la escritura a la salida de error en Clojure es sencilla, ya que se puede utilizar la función "System/err" para imprimir mensajes a la salida de error estándar.

## Ver también:

- [¿Qué es la salida de error estándar?](https://en.wikipedia.org/wiki/Standard_error)