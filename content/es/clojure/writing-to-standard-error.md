---
title:                "Clojure: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

Escribir a la salida de error estándar (standard error) es una herramienta útil para los programadores en Clojure. Nos permite imprimir información de depuración en tiempo de ejecución y mostrar errores que pueden ser cruciales para el correcto funcionamiento de nuestras aplicaciones.

## Cómo hacerlo

Para escribir a la salida de error estándar en Clojure, se utiliza la función `println` junto con `System/err`. Aquí un ejemplo sencillo:

```Clojure
(println "Este es un mensaje de error" System/err)
```

El resultado de ejecutar este código sería:

```Clojure
Este es un mensaje de error
nil
```

Podemos ver que el mensaje se imprime en rojo, lo que indica que es un mensaje de error. También es importante mencionar que la palabra `nil` se imprime después del mensaje, esto se debe a que `println` siempre devuelve `nil` después de imprimir su argumento.

## Profundizando

Existen diferentes formas de escribir a la salida de error estándar en Clojure, una de ellas es utilizando la macro `println`, que nos permite imprimir múltiples argumentos separados por espacios. Otra opción es utilizar la función `format` que nos permite crear mensajes personalizados utilizando patrones de formato.

También es importante mencionar que la salida de error estándar es diferente de la salida estándar (standard output) en Clojure. Mientras que la salida estándar se utiliza para imprimir información relevante al usuario, la salida de error estándar es específica para mensajes de error y depuración.

## Ver también

- [Documentación oficial de Clojure](https://clojure.org/)
- [Cómo imprimir a la salida de error en Clojure](https://stackoverflow.com/questions/8122002/how-do-i-print-to-standard-error-in-clojure)