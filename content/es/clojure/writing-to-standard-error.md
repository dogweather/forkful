---
title:                "Clojure: Escribiendo en el error estándar"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Por qué escribir al error estándar en Clojure

Escribir al error estándar es una práctica común en la programación en Clojure. Al hacerlo, podemos dejar mensajes de error o información de depuración en la consola, lo que nos ayuda a identificar y solucionar problemas en nuestro código.

## Cómo hacerlo

Para escribir al error estándar en Clojure, utilizamos la función `prn`, que imprimirá una línea de texto seguida de una nueva línea en la consola. Podemos pasar cualquier tipo de dato a esta función, desde un string hasta una lista. Por ejemplo:

```Clojure 
(prn "Este es un mensaje de error")
;; resultado: "Este es un mensaje de error"

(prn [1 2 3 4])
;; resultado: [1 2 3 4]
```

También podemos utilizar la función `println` para escribir al error estándar, que funciona de manera similar a `prn`, pero agrega un espacio entre los elementos impresos.

```Clojure
(println "Este es un mensaje" "de varias palabras")
;; resultado: Este es un mensaje de varias palabras
```

## Profundizando

Además de `prn` y `println`, también podemos escribir al error estándar utilizando la función `eprintln`, que funciona igual que `println` pero agrega el prefijo "error" en la consola. Otra opción es utilizar la función `printf`, que nos permite formatear el texto que se imprime en la consola.

```Clojure
(eprintln "Este es un mensaje" "de error")
;; resultado: error Este es un mensaje de error

(printf "Este es un número: %d" 5)
;; resultado: Este es un número: 5
```

También podemos usar `with-out-str` para capturar el texto impreso en la consola en una variable, en lugar de imprimirlo directamente.

```Clojure
(def error (with-out-str (prn "Este es un mensaje de error")))
;; resultado: nil

(println error)
;; resultado: Este es un mensaje de error
```

# Ver también

- Documentación oficial de Clojure sobre `prn`: https://clojuredocs.org/clojure.core/prn
- Información sobre `with-out-str`: https://clojuredocs.org/clojure.core/with-out-str