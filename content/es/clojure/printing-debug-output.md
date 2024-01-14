---
title:                "Clojure: Imprimiendo salida de depuración"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

Imprimir mensajes de depuración es una técnica comúnmente utilizada en programación para identificar y solucionar errores en el código. Esta práctica permite a los desarrolladores obtener una mejor comprensión de cómo se están ejecutando sus programas y qué valores están siendo procesados en cada paso. En esta publicación, aprenderemos cómo imprimir mensajes de depuración en Clojure y cómo puede ayudarnos en nuestro proceso de desarrollo.

## Cómo hacerlo

Imprimir mensajes de depuración en Clojure es muy sencillo. Simplemente podemos utilizar la función `println` y pasarle como argumento el valor que queremos imprimir. Por ejemplo:

```Clojure
(let [num 5]
  (println "El valor de num es:" num))
```

La salida de este código sería:

```
El valor de num es: 5
```

Podemos incluso imprimir varios valores en una sola línea, separándolos por comas:

```Clojure
(println "El valor de num es:" num ", y el valor de otro es:" otro)
```

La salida de este código sería:

```
El valor de num es: 5, y el valor de otro es: "texto"
```

## Profundizando

Además de `println`, también podemos utilizar otras funciones como `pr` y `pprint` para imprimir mensajes de depuración en diferentes formatos. Por ejemplo, `pr` imprime de forma más legible para los humanos, mientras que `pprint` nos permite controlar la indentación y la presentación de los datos impresos. También podemos utilizar la macro `ensure` para imprimir un mensaje de error si una condición no se cumple. Por ejemplo:

```Clojure
(ensure condition "Error: La condición no se cumple")
```

Otra técnica útil es utilizar el operador `>>` para imprimir un valor y continuar ejecutando el código. Por ejemplo:

```Clojure
(>> (println "Este mensaje se imprimirá")
    (println "Este también")
    (+ 1 2))
```

La salida de este código sería:

```
Este mensaje se imprimirá
Este también
3
```

## Ver también

- [Documentación oficial de Clojure sobre mensajes de depuración](https://clojure.org/reference/other_functions#Debugging_Forms)
- [Artículo sobre técnicas avanzadas de depuración en Clojure](https://medium.com/@vvkchandra/top-5-debugging-techniques-in-clojure-ba31de40b829)
- [Artículo sobre la importancia de los mensajes de depuración en el proceso de desarrollo](https://blog.jetbrains.com/idea/2012/09/why-debug/)