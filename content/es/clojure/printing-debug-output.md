---
title:                "Impresión de salida de depuración"
html_title:           "Clojure: Impresión de salida de depuración"
simple_title:         "Impresión de salida de depuración"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un desarrollador, es probable que en algún momento hayas tenido que lidiar con errores en tu código. Aquí es donde imprimir la salida de depuración puede ser una herramienta valiosa para ayudarte a identificar y resolver estos errores más rápido. ¡Sigue leyendo para descubrir cómo aprovechar al máximo esta función en Clojure!

## Cómo

Imprimir la salida de depuración en Clojure es muy sencillo. Puedes utilizar la función `prn` para imprimir cualquier valor en la consola. Por ejemplo:

```Clojure
(prn "Hola Mundo")
```

Esto imprimirá `Hola Mundo` en la consola. También puedes imprimir variables y estructuras de datos más complejas, como listas o mapas:

```Clojure
(def temperaturas [32 45 60 72 85])

(prn temperaturas)
```

Esto imprimirá la lista de temperaturas en la consola, lo que puede ser útil si estás depurando un algoritmo que trabaja con esta lista.

## Deep Dive

La función `prn` es útil, pero tiene algunas limitaciones. Por ejemplo, solo puede imprimir en la consola y no permite personalizar la forma en que se imprime el valor. Si quieres más control sobre la salida de depuración, puedes utilizar la función `println` en su lugar.

`println` te permite imprimir en cualquier flujo de salida, como archivos o sockets. También te permite formatear la salida como desees. Por ejemplo, si quieres imprimir una variable junto con su nombre, puedes hacer lo siguiente:

```Clojure
(println "La temperatura actual es:" temperatura-actual)
```

Esto imprimirá en la consola `La temperatura actual es: 72`, suponiendo que `temperatura-actual` tiene un valor de 72.

## Ver también

- [Documentación oficial de Clojure sobre impresión de valores en la consola] (https://clojure.org/reference/other_functions#Printing%20values%20to%20the%20console)
- [Artículo sobre cómo depurar en Clojure] (https://medium.com/@AdamsChar/clojure-debugging-fundamentals-c82afc17644f)
- [Curso de depuración en Clojure] (https://purelyfunctional.tv/courses/debugging-in-clojure/)