---
title:                "Encontrando la longitud de una cadena"
html_title:           "Clojure: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Si te estás iniciando en la programación con Clojure, es posible que te preguntes por qué es importante saber cómo encontrar la longitud de una cadena de texto. La realidad es que esta es una habilidad fundamental en cualquier lenguaje de programación, ya que te permite manipular y trabajar con datos de texto de manera efectiva.

## ¿Cómo hacerlo?

En Clojure, podemos encontrar la longitud de una cadena de texto utilizando la función `count`. Esta función toma una cadena de texto como argumento y devuelve su longitud como un número entero.

```Clojure
(count "Hola mundo") ; Salida: 10
(count "") ; Salida: 0
```

En el primer ejemplo, se cuenta la longitud de la cadena "Hola mundo", que consta de 10 caracteres (incluyendo el espacio en blanco). En el segundo ejemplo, se cuenta la longitud de una cadena vacía, que obviamente es 0.

Otra forma de encontrar la longitud de una cadena de texto es convirtiéndola en una secuencia y luego utilizando la función `count` como se muestra a continuación:

```Clojure
(count (seq "Hola mundo")) ; Salida: 10
```

Esta segunda opción puede ser útil si necesitas trabajar con caracteres específicos de la cadena.

## Buceando más profundo

¿Sabías que la función `count` también se puede utilizar en otros tipos de datos? En Clojure, se puede aplicar en listas, vectores, conjuntos y mapas para obtener su tamaño.

```Clojure
(count [1 2 3]) ; Salida: 3
(count #{:a :b :c}) ; Salida: 3
```

También es importante tener en cuenta que la función `count` es muy eficiente y no realiza un recorrido completo de la secuencia. En lugar de eso, utiliza la información de longitud ya almacenada en los datos para devolver el resultado, lo que la hace muy útil cuando se trabaja con grandes conjuntos de datos.

## Ver también

- Documentación oficial de la función `count`: https://clojuredocs.org/clojure.core/count
- Tutoriales gratuitos de Clojure en línea: https://www.braveclojure.com/
- Comunidad de programadores en español de Clojure: https://clojured.es/