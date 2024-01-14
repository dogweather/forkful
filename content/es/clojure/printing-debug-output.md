---
title:                "Clojure: Imprimiendo la salida de depuración"
simple_title:         "Imprimiendo la salida de depuración"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué imprimir salidas de depuración en Clojure

Imprimir salidas de depuración en el código es una técnica fundamental para identificar errores y solucionar problemas en nuestra aplicación. En este post, te mostraremos cómo utilizar esta herramienta en tus proyectos de Clojure y cómo te puede ayudar en tu proceso de desarrollo.

## Cómo hacerlo

Imprimir salidas de depuración es bastante simple en Clojure. Solo necesitas utilizar la función `println` para imprimir el valor de una variable o una expresión en la consola. Por ejemplo:

```Clojure
(def usuario "Juan")
(println usuario)
```

Este código imprimirá en la consola "Juan" como resultado. Además, puedes utilizar la función `prn` para imprimir de forma más legible ciertos tipos de datos, como listas o mapas. Por ejemplo:

```Clojure
(def mascotas ["gato" "perro" "conejo"])
(prn mascotas)
```

Este código imprimirá en la consola "(\"gato\" \"perro\" \"conejo\")" como resultado.

## Inmersión profunda

Existen diferentes técnicas y métodos para imprimir salidas de depuración en Clojure. Por ejemplo, también puedes utilizar la macro `spit` para escribir la salida en un archivo de texto o la librería `tools.logging` para imprimir mensajes de error con diferentes niveles de severidad.

Además, es importante tener en cuenta que imprimir salidas de depuración puede tener un impacto en el rendimiento de tu aplicación, por lo que es recomendable utilizarlo solo en casos necesarios y eliminarlo en la versión final del código.

## Ver también

- [Documentación oficial de print y println en Clojure](https://clojure.github.io/clojure/clojure.core-api.html#print)
- [Uso de spit en Clojure](https://clojuredocs.org/clojure.core/spit)
- [Librería tools.logging en Clojure](https://github.com/clojure/tools.logging)