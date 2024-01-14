---
title:                "Clojure: Leyendo argumentos de línea de comandos"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Por qué leer argumentos de línea de comandos en Clojure?

Si estás familiarizado con Clojure, probablemente ya sepas que es un lenguaje de programación funcional basado en la JVM. Pero, ¿sabías que también puedes usarlo para leer argumentos de línea de comandos? Esta es una habilidad útil si deseas crear programas que puedan recibir inputs personalizados de los usuarios al ser ejecutados desde la terminal.

## Cómo leer argumentos de línea de comandos en Clojure

Para leer argumentos de línea de comandos en Clojure, puedes usar la función `command-line-args` que viene incluida en el paquete `clojure.main`. Esta función regresa una lista con todos los argumentos pasados a través de la terminal al momento de ejecutar tu programa.

```
Clojure (command-line-args)
;; Si ejecutas este código desde la terminal con el input "my-program 1 2 3", el resultado será: ["my-program" "1" "2" "3"]
```

Puedes acceder a cada argumento de la lista usando índices numéricos. Por ejemplo, `args` es el primer argumento en la lista, `args[0]` es el segundo, y así sucesivamente. A continuación te mostramos un ejemplo de cómo usar esta función en un programa que reciba dos argumentos y los sume:

```
Clojure
(defn sum-args
  (let [args (command-line-args)]
    (println (+ (Integer/parseInt (args[0])) (Integer/parseInt (args[1]))))))

(sum-args)
;; Si ejecutas este código desde la terminal con los inputs "my-program 1 2", la salida será: 3
```

## Profundizando en la lectura de argumentos de línea de comandos en Clojure

Existen algunas cosas a tener en cuenta cuando se utilizan argumentos de línea de comandos en Clojure:

- Puedes usar la función `count` para determinar el número de argumentos pasados en la lista. Esto puede ser útil si tu programa necesita un número específico de inputs.
- También puedes usar la función `getopts` para obtener opciones específicas de los argumentos, como flags o parámetros con valores asignados.
- Los argumentos pasados a través de la terminal siempre serán interpretados como cadenas de texto, por lo que si necesitas usar números, deberás convertirlos a enteros o decimales utilizando funciones como `Integer/parseInt` o `Float/parseFloat`.

En resumen, leer argumentos de línea de comandos en Clojure es una habilidad útil para crear programas más interactivos y personalizables.

## Ver También
- Documentación oficial de Clojure: https://clojure.org/reference/command_line_tools
- Tutorial de lectura de argumentos de línea de comandos en Clojure: https://www.tutorialspoint.com/clojure/clojure_command_line_arguments.htm
- Ejemplos prácticos de lectura de argumentos de línea de comandos en Clojure: https://www.geeksforgeeks.org/read-command-line-arguments-in-clojure/