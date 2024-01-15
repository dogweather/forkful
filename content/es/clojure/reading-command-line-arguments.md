---
title:                "La lectura de los argumentos en la línea de comandos"
html_title:           "Clojure: La lectura de los argumentos en la línea de comandos"
simple_title:         "La lectura de los argumentos en la línea de comandos"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

Al programar en Clojure, es posible que en algún momento necesites trabajar con argumentos de línea de comandos. Esto puede ser útil para pasar información al programa o para realizar ciertas acciones dependiendo de los argumentos recibidos. En este artículo, aprenderemos cómo leer y trabajar con los argumentos de línea de comandos en Clojure.

## Cómo hacerlo

Para leer los argumentos de línea de comandos en Clojure, podemos utilizar la función `clojure.core/command-line-args`. Esta función toma como argumento una lista de argumentos y devuelve una lista de cadenas con los argumentos recibidos. Veamos un ejemplo de cómo podemos utilizar esta función:

```Clojure
(defn leer-argumentos [args]
  (let [argumentos (clojure.core/command-line-args args)]
    (println "Los argumentos recibidos son:" argumentos)))

(leer-argumentos *command-line-args*)
```

En este ejemplo, estamos definiendo una función `leer-argumentos` que toma como argumento una lista de argumentos. Luego, utilizamos la función `clojure.core/command-line-args` para obtener la lista de argumentos recibidos y la almacenamos en una variable llamada `argumentos`. Finalmente, imprimimos la lista de argumentos recibidos utilizando la función `println`.

Si ejecutamos este código pasándole algunos argumentos de línea de comando, por ejemplo `clojure ejemplo.clj arg1 arg2 arg3`, obtendremos como salida:

```
Los argumentos recibidos son: [arg1 arg2 arg3]
```

Como podemos ver, la función `clojure.core/command-line-args` devuelve una lista de cadenas con los argumentos recibidos. Ahora, podemos utilizar esta lista para realizar las acciones que necesitemos en nuestro programa.

## Profundizando

La función `clojure.core/command-line-args` también nos permite especificar un prefijo para los argumentos. Por defecto, el prefijo es `-`. Sin embargo, podemos cambiarlo pasando un argumento adicional a la función. Por ejemplo, si queremos utilizar `--` como prefijo, podemos hacerlo de la siguiente manera:

```Clojure
(defn leer-argumentos [args]
  (let [argumentos (clojure.core/command-line-args args "--")]
    (println "Los argumentos recibidos son:" argumentos)))

(leer-argumentos *command-line-args*)
```

Ahora, si ejecutamos este código con los argumentos `clojure ejemplo.clj --arg1 --arg2 --arg3`, obtendremos como salida:

```
Los argumentos recibidos son: [arg1 arg2 arg3]
```

También podemos utilizar la función `clojure.core/command-line-args` para parsear argumentos con valores. Por ejemplo, si queremos recibir un argumento con un valor numérico, podemos utilizar la siguiente función:

```Clojure
(defn leer-argumentos [args]
  (let [argumentos (clojure.core/command-line-args args)]
    (println "El argumento recibido es:" (Integer/parseInt (second argumentos)))))

(leer-argumentos *command-line-args*)
```

Al ejecutar este código con el argumento `clojure ejemplo.clj -valor 10`, obtendremos como salida:

```
El argumento recibido es: 10
```

Como podemos ver, podemos utilizar la función `clojure.core/command-line-args` para trabajar con diferentes tipos de argumentos y valores.

## Ver también

- Documentación oficial de la función `clojure.core/command-line-args`: https://clojuredocs.org/clojure.core/command-line-args
- Ejemplo de uso de la función `clojure.core/command-line-args`: https://clojuredocs.org/clojure.core/with-open#example-54269250e4b04b050a02b4ef