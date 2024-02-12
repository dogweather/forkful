---
title:                "Escribiendo pruebas"
aliases: - /es/clojure/writing-tests.md
date:                  2024-02-03T19:29:57.963990-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escribiendo pruebas"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir pruebas en Clojure, al igual que en otros lenguajes de programación, implica crear código dedicado para verificar que tu código principal funciona como se espera. Ayuda a asegurar la corrección, facilita la refactorización y mejora la estabilidad del código.

## Cómo hacerlo:
Clojure, aprovechando la JVM, soporta varios marcos de pruebas. Sin embargo, una biblioteca integrada comúnmente utilizada es `clojure.test`. Aquí hay un ejemplo simple:

```clojure
(ns example.test
  (:require [clojure.test :refer :all]
            [example.core :refer :all]))

(deftest test-addition
  (testing "Funcionalidad de adición"
    (is (= 4 (add 2 2)))
    (is (= 7 (add 3 4)))))

(run-tests)
```
Después de ejecutar esta prueba, verías una salida similar a:

```
Probando example.test

Se ejecutaron 2 pruebas que contienen 2 afirmaciones.
0 fallos, 0 errores.
```

Para aquellos que buscan opciones más ricas en características, se pueden utilizar bibliotecas de terceros como `Midje` o `test.check`. Así es como podrías usar Midje para una prueba similar:

Primero, añade Midje a las dependencias de tu project.clj:
```clojure
[midje "1.9.9"]
```

Luego, tu prueba con Midje podría lucir así:

```clojure
(ns example.test
  (:require [midje.sweet :refer :all]
            [example.core :refer :all]))

(fact "Probando adicion"
  (add 2 2) => 4
  (add 3 4) => 7)
```

Al ejecutar la prueba a través de Midje con `lein midje`, la salida mostraría algo parecido a:

```
Todas las comprobaciones (2) tuvieron éxito.
```
