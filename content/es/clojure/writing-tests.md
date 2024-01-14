---
title:    "Clojure: Redactando pruebas"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## ¿Por qué escribir pruebas en Clojure es importante?

Escribir pruebas en Clojure puede ahorrar tiempo y esfuerzo a largo plazo. Ayuda a detectar errores y problemas de código antes de que se presenten en un entorno de producción, lo que permite resolverlos de manera rápida y eficiente.

## ¿Cómo hacerlo?

```Clojure
(ns tests.core
  (:require [clojure.test :refer :all]
            [my.core :refer :all])) ; importar el código a ser probado

(deftest test-suma
  (is (= 6 (suma 2 4)))) ; afirmar que 2 + 4 = 6

(deftest test-resta
  (is (= 2 (resta 4 2)))) ; afirmar que 4 - 2 = 2

(deftest test-multiplicacion
  (is (= 12 (multiplicacion 3 4)))) ; afirmar que 3 * 4 = 12

(run-tests) ; ejecutar todas las pruebas definidas
```

La salida esperada debe ser:

```
Ran 3 tests containing 3 assertions.
0 failures, 0 errors.
```

## Profundizando

Al escribir pruebas en Clojure, se recomienda seguir el principio "AAA" (Arrange, Act, Assert). Primero, se establecen las condiciones iniciales o "Arrange". Luego, se ejecuta el código a probar o "Act". Y finalmente, se verifica el resultado esperado o "Assert".

Además, Clojure cuenta con una biblioteca de pruebas integrada llamada `clojure.test`, que proporciona funciones útiles como `is`, `are`, y `deftest`. También es posible utilizar otras bibliotecas externas, como `speclj` y `expectations`, para escribir pruebas con un estilo más descriptivo y legible.

Finalmente, es importante recordar que escribir pruebas no garantiza un código perfecto, pero puede ayudar a mejorar la calidad y la robustez del mismo. Es una práctica importante para cualquier equipo de desarrollo que busque mantener un alto nivel de calidad en su código.

## Ver también

- [Documentación de pruebas en Clojure](https://clojure.org/guides/testing)
- [Ejemplos de pruebas en Clojure](https://github.com/clojure/clojurescript/blob/master/test/clj/cljs/test_cljs.clj)
- [Biblioteca externa para pruebas en Clojure: speclj](https://github.com/slagyr/slagyr/wiki/Getting-Started-With-speclj)
- [Biblioteca externa para pruebas en Clojure: expectations](https://github.com/clojure-expectations/clojure-autoexpect)