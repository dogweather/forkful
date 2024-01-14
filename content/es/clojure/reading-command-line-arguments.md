---
title:    "Clojure: Leyendo argumentos de línea de comandos"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por qué

La lectura de argumentos de línea de comando es una habilidad esencial para cualquier desarrollador de Clojure. Esta técnica permite que nuestros programas sean más interactivos y flexibles al permitirnos proporcionar información al programa al ejecutarlo en lugar de estar codificada directamente en el código.

## Cómo hacerlo

La forma más sencilla de leer argumentos de línea de comando en Clojure es mediante el uso de la función `clojure.core/command-line-args`. Esta función toma todos los argumentos proporcionados al ejecutar el programa y los devuelve en una lista. Veamos un ejemplo:

```Clojure
(def args (command-line-args))
(println "El primer argumento es:" (first args))
```

Si ejecutamos este código en la línea de comando con el argumento "hola", veremos el siguiente resultado:

```
El primer argumento es: hola
```

También podemos acceder a argumentos específicos proporcionando su posición en la lista, como `args[1]` en otros lenguajes. Por ejemplo:

```Clojure
(println "El tercer argumento es:" (nth args 2))
```

Con el mismo argumento "hola", obtendríamos:

```
El tercer argumento es: lo
```

También vale la pena mencionar que podemos usar la función `count` para verificar cuántos argumentos se proporcionaron, y `rest` para obtener una lista de todos los argumentos excepto el primero. ¡Juega un poco con estas funciones para familiarizarte con ellas y su comportamiento!

## Profundizando

Para aquellos que quieran profundizar en la lectura de argumentos de línea de comando en Clojure, existen varias bibliotecas de terceros que facilitan esta tarea. Algunas de las más populares son `cljoptparse` y `joker`. Estas bibliotecas ofrecen una sintaxis más amigable y funciones más avanzadas para manejar distintas situaciones en las que se puedan presentar argumentos duplicados o argumentos con valores asociados.

Es importante tener en cuenta que, si bien es útil poder leer argumentos de línea de comando en nuestros programas, también es importante validarlos correctamente e informar al usuario sobre cómo usarlos adecuadamente.

## Ver también

- [cljoptparse](https://github.com/clojusc/cljoptparse)
- [joker](https://github.com/zcaudate/joker)
- [Función `command-line-args`](https://clojuredocs.org/clojure.core/command-line-args)