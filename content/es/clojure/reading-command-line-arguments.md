---
title:                "Clojure: Leyendo argumentos de línea de comando."
simple_title:         "Leyendo argumentos de línea de comando."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

Muchos desarrolladores y programadores pueden estar familiarizados con la idea de pasar argumentos de línea de comandos a un programa o script, pero ¿por qué alguien querría hacer esto en Clojure? La respuesta es simple: al leer argumentos de línea de comandos, podemos hacer que nuestro programa sea más dinámico y personalizable para el usuario.

## Cómo hacerlo

Para leer los argumentos de línea de comandos en Clojure, podemos usar la función `*command-line-args*`. Esta función devuelve una lista de todos los argumentos pasados a nuestro programa desde la línea de comandos, incluyendo el nombre del programa en sí. Por ejemplo:

```Clojure
(defn saludo [nombre]
  (println "¡Hola" nombre "! Bienvenido a mi programa."))

(defn -main []
  (doseq [arg *command-line-args*]
    (saludo arg)))
```

Si ejecutamos este código en la línea de comandos con el siguiente comando:

`$ java -cp <ruta_a_jar> programa.clj Juan Pedro Marta`

Obtendremos la siguiente salida:

```
¡Hola programa ! Bienvenido a mi programa.
¡Hola Juan ! Bienvenido a mi programa.
¡Hola Pedro ! Bienvenido a mi programa.
¡Hola Marta ! Bienvenido a mi programa.
```

Podemos ver que la lista de argumentos de línea de comandos se imprime en el orden en que fueron pasados al programa.

## Profundizando

Además de la función `*command-line-args*`, también podemos usar la biblioteca `clojure.tools.cli` para realizar un análisis más sofisticado de los argumentos de línea de comandos. Esta biblioteca proporciona una forma más fácil de definir argumentos específicos, como banderas o valores opcionales, y manejar errores de entrada de manera más elegante. Puedes encontrar más información y ejemplos en la [documentación oficial de Clojure](https://clojure.github.io/tools.cli/).

## Ver también

- [Documentación oficial de Clojure sobre *command-line-args*](https://clojuredocs.org/clojure.core/*command-line-args*)
- [Documentación oficial de Clojure sobre `clojure.tools.cli`](https://clojure.github.io/tools.cli/)