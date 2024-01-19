---
title:                "Imprimiendo salida de depuración"
html_title:           "Arduino: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

# "Imprimir la Salida del Depurador en Clojure"

## ¿Qué & Por Qué?  
Imprimir la salida del depurador es una técnica en la que los programadores producen mensajes informativos en la consola para entender el funcionamiento interno de un programa. Hacemos esto para detectar errores y comprender mejor cómo se ejecuta nuestro código.

## ¿Cómo hacerlo?
Vamos a utilizar la función ```println``` incorporada en el lenguaje Clojure.

```Clojure
(defn hello-world []
  (println "¡Hola, Mundo!"))

(hello-world)
```

Ejecutando este código tendrá la siguiente salida:

```Clojure
¡Hola, Mundo!
```
También puedes imprimir varias cosas a la vez:

```Clojure
(defn print-numbers []
  (println "Los números son:" 1 2 3 4))

(print-numbers)
```

La salida será:

```Clojure
Los números son: 1 2 3 4
```

## Inmersión Profunda

1. **Contexto Histórico**: Desde los primeros días de la programación, la depuración ha sido esencial. Aunque los depuradores visuales modernos a menudo ofrecen una interfaz gráfica de usuario y puntos de ruptura, imprimir la salida todavía se utiliza debido a su simplicidad y conveniencia. 

2. **Alternativas**: Clojure tiene librerías como 'tools.logging' y 'seesaw' para manejar registros más complejos. Estas librerías ofrecen más control y se ajustan mejor a aplicaciones en producción.

3. **Detalles de Implementación**: En Clojure, `println` imprime a `*out*`, el "valor var" actual de la salida estándar. Puedes redirigir la salida a un archivo o a otra ubicación si lo prefieres. Por ejemplo:
```Clojure
(binding [*out*  (java.io.PrintWriter. "archivo.txt")]
  (println "hola"))
```

## Vea También

1. [Función println en Clojure](https://clojuredocs.org/clojure.core/println)
2. [Librería 'tools.logging'](https://github.com/clojure/tools.logging)
3. [Librería 'seesaw'](https://github.com/daveray/seesaw)