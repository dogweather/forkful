---
title:                "Redactar pruebas"
html_title:           "Clojure: Redactar pruebas"
simple_title:         "Redactar pruebas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir pruebas en la programación es crear código para verificar si nuestro código funciona correctamente. Los programadores lo hacen para asegurarse de que su código está libre de errores y funcione como se espera.

## ¿Cómo hacerlo?

### Ejemplo básico de prueba

```Clojure
(defn suma [a b]
  (+ a b))

(assert (= (suma 2 3) 5)) ;; Esto es una prueba que verificará si la función suma devuelve el resultado esperado, en este caso, 5.
```

### Ejemplo usando la biblioteca `clojure.test`

```Clojure
(use 'clojure.test)

(defn es-positivo? [num]
  (if (< num 0)
    false
    true))

(deftest test-es-positivo?
  (is (= (es-positivo? 5) true))
  (is (= (es-positivo? -10) false))
  (is (= (es-positivo? 0) true)))

(run-tests) ;; Esto ejecutará todas las pruebas definidas y mostrará un informe con los resultados.
```

## Profundizando

Escribir pruebas se originó en la metodología de programación llamada "Desarrollo Guiado por Pruebas" (TDD). Este enfoque consiste en escribir pruebas primero y luego escribir el código correspondiente para que pase las pruebas. También se puede utilizar "Desarrollo impulsado por Pruebas" (BDD), que se centra en las características y comportamientos del código en lugar de su funcionalidad.

Como alternativa a escribir pruebas manualmente, también existen herramientas de generación automática de pruebas, como `clojure.test.generative`, que utiliza generación aleatoria de datos para probar el código.

La implementación de pruebas en Clojure se realiza utilizando la biblioteca `clojure.test` o cualquier otra biblioteca de pruebas externa. Se pueden crear pruebas en cualquier archivo de Clojure y ejecutarlas desde la línea de comandos utilizando `clojure` junto con el archivo.

## Ver también

- [Documentación de clojure.test](https://clojure.github.io/clojure/clojure.test-api.html)
- [Desarrollo impulsado por Pruebas (BDD)](https://vimeo.com/105770914)