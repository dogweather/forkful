---
title:                "Escribiendo pruebas"
html_title:           "Clojure: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/writing-tests.md"
---

{{< edit_this_page >}}

# ¿Por qué escribir pruebas en Clojure?

La escritura de pruebas en Clojure es una práctica esencial para garantizar el buen funcionamiento y la calidad del código. Las pruebas permiten detectar errores y bugs de manera temprana, lo que ahorra tiempo y esfuerzo en el proceso de desarrollo. Además, escribir pruebas ayuda a mantener un código más limpio y organizado.

## Cómo hacerlo

Para escribir pruebas en Clojure, utilizaremos la biblioteca de pruebas integrada en el lenguaje, llamada clojure.test. Primero, debemos importarla en nuestro código con la siguiente línea:

```clojure
(require '[clojure.test :refer [deftest is]])
```

Luego, podemos definir nuestras pruebas utilizando la función `deftest`. En el siguiente ejemplo, estamos probando una función simple que suma dos números enteros:

```clojure
(deftest suma-prueba
  (is (= 4 (+ 2 2))))
```

En este caso, utilizamos la función `is` para comparar el resultado de la suma con el valor esperado (4) utilizando la función de igualdad `=`. Si la prueba es exitosa, no se mostrará ningún mensaje en la consola. De lo contrario, se imprimirá un mensaje de error indicando cuál fue el valor esperado y cuál fue el valor obtenido.

Podemos ejecutar nuestras pruebas utilizando la función `run-tests`. Esta función acepta una lista de nombres de pruebas o una expresión regular como argumento. Por ejemplo:

```clojure
(run-tests 'suma-prueba) ; ejecuta solo la prueba "suma-prueba"
(run-tests #"\w+-prueba") ; ejecuta todas las pruebas que terminen con "-prueba"
```

## Profundizando

Además de la función `is`, clojure.test también nos provee de otras funciones útiles para escribir pruebas. Algunas de ellas son:

- `testing`: nos permite agrupar nuestras pruebas en una sección con un nombre descriptivo.
- `isnt`: compara dos valores y verifica que sean diferentes.
- `throws?`: verifica que una determinada excepción sea lanzada por una función.
- `are`: nos permite verificar múltiples assertiones en una misma prueba.

Además, podemos utilizar la macro `deftest+` para definir pruebas parametrizadas, lo que nos permite probar una función con diferentes valores de entrada.

En resumen, escribir pruebas en Clojure es una práctica esencial para garantizar la calidad y el buen funcionamiento de nuestro código. Con la biblioteca de pruebas integrada en el lenguaje, podemos escribir pruebas de manera sencilla y efectiva, lo que nos permite detectar errores tempranamente y mantener nuestro código limpio y organizado.

# Ver también

- [Documentación oficial de clojure.test](https://clojuredocs.org/clojure.test)
- [Ejemplo de pruebas en Clojure](https://github.com/clojure/tools.namespace/blob/master/test/src/clojure/tools/namespace/test/namespace_test.clj)