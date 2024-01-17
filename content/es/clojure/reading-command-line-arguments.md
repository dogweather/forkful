---
title:                "Leyendo argumentos de línea de comandos"
html_title:           "Clojure: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Leer argumentos de línea de comando es una técnica utilizada en programación para obtener información de la línea de comando en la que se ejecuta un programa. Los programadores lo hacen para obtener información específica del usuario antes de que el programa se ejecute.

## Cómo:

```
Clojure -m tu_programa.clj arg1 arg2
```

El código anterior ejecuta el programa "tu_programa.clj" y pasa los argumentos "arg1" y "arg2" a dicho programa. Dentro del programa, puedes acceder a los argumentos utilizando la función "command-line-args".

```
Clojure -m tu_programa.clj -n
```

En este caso, el argumento "-n" es una flag o indicador que puede ser utilizado para realizar una acción específica dentro del programa. Para acceder a las flags, se puede utilizar la función "rest".

## Profundizando:

Leer argumentos de línea de comando es una técnica que se ha utilizado desde los primeros días de la programación en sistemas operativos como UNIX. Actualmente, existen otras formas de obtener información del usuario, como por ejemplo a través de interfaces gráficas, pero leer argumentos de línea de comando sigue siendo una herramienta muy útil y fácil de implementar en programas de línea de comandos.

## Ver también:

Para más información puedes visitar la documentación oficial de Clojure sobre lectura de argumentos de línea de comando: https://clojure.org/reference/java_interop#command-line-arguments