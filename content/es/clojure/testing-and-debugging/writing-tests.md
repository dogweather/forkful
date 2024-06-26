---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:57.963990-07:00
description: "C\xF3mo hacerlo: Clojure, aprovechando la JVM, soporta varios marcos\
  \ de pruebas. Sin embargo, una biblioteca integrada com\xFAnmente utilizada es\u2026"
lastmod: '2024-03-13T22:44:58.660314-06:00'
model: gpt-4-0125-preview
summary: Clojure, aprovechando la JVM, soporta varios marcos de pruebas.
title: Escribiendo pruebas
weight: 36
---

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
