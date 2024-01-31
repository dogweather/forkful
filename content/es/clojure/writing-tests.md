---
title:                "Escribiendo pruebas"
date:                  2024-01-19
html_title:           "Arduino: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"

category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Escribir pruebas es crear casos automáticos para validar que tu código hace lo que esperas. Los programadores prueban para evitar errores, garantizar calidad y facilitar mantenimiento.

## Cómo hacerlo:
Clojure usa la biblioteca `clojure.test` para pruebas. Un ejemplo sencillo:

```clojure
(ns mi-proyecto.core-test
  (:require [clojure.test :refer :all]
            [mi-proyecto.core :refer :all]))

(deftest prueba-suma
  (testing "Probar suma de dos números"
    (is (= 4 (sumar 2 2)))))

(run-tests)

```

La salida de la prueba sería algo así:

```clojure
Testing mi-proyecto.core-test

Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

## Profundización
Las pruebas en Clojure vienen de la tradición de TDD (Test-Driven Development) en Smalltalk y han sido populares desde los años 90. Alternativas a `clojure.test` incluyen `Midje` y `Speclj`, ofreciendo una experiencia más rica y DSL (Domain-Specific Languages). Para implementar pruebas, se sigue el ciclo rojo-verde-refactor: escribes una prueba que falla, escribes código que hace que la prueba pase, y luego refinas el código manteniendo las pruebas pasando.

## Ver Además
Para más recursos de pruebas en Clojure, puedes visitar:

- La [Página oficial de clojure.test](https://clojure.github.io/clojure/clojure.test-api.html)
- Un [tutorial de `Midje`](https://github.com/marick/Midje/)
- Documentación de [`Speclj`](http://speclj.com/)
- [Blog de Martin Fowler](https://martinfowler.com/bliki/TestDrivenDevelopment.html) sobre TDD.
