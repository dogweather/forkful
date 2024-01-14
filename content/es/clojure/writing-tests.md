---
title:    "Clojure: Escribiendo pruebas"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/clojure/writing-tests.md"
---

{{< edit_this_page >}}

# Por qué escribir pruebas en Clojure

Escribir pruebas en Clojure es una práctica importante para garantizar que nuestro código funcione correctamente y prevenir errores en nuestro programa. Además, nos ayuda a detectar y corregir posibles bugs en etapas tempranas del desarrollo, ahorrando tiempo y esfuerzo en el futuro.

## Cómo hacerlo

Para escribir pruebas en Clojure, utilizamos la biblioteca de testing integrada en el lenguaje, llamada `clojure.test`. Primero, definimos una función de prueba utilizando la macro `deftest` y dentro de ella escribimos los casos de prueba utilizando la macro `is`.

```Clojure
(deftest test-sum
  (is (= 4 (+ 2 2))) ; esto debería ser verdadero
  (is (not= 5 (+ 3 3))) ; esto debería ser falso
)
```

Una vez que tengamos nuestras pruebas definidas, podemos ejecutarlas utilizando la función `run-tests` y obteniendo un informe sobre qué pruebas se pasaron y cuáles fallaron.

## Profundizando

Cuando escribimos pruebas en Clojure, es importante mantener ciertas buenas prácticas para un código más limpio y fácil de mantener a largo plazo. Algunas de estas prácticas incluyen:

- Utilizar nombres descriptivos para nuestras funciones de prueba y casos de prueba, para que sea más fácil entender su propósito.
- Buscar casos de prueba extremos y esquinas para asegurarnos de que nuestro código maneje todas las situaciones posibles.
- Utilizar la función `testing` para agrupar nuestras pruebas en contextos lógicos.

Además, podemos utilizar bibliotecas externas como `core-spec` para añadir anotaciones y especificaciones a nuestras pruebas, lo que nos ayuda a documentar nuestro código y a tener una mayor comprensión de lo que estamos probando.

# Ver también

- Documentación oficial de `clojure.test`: https://clojure.github.io/clojure/clojure.test-api.html
- Ejemplo de pruebas en Clojure: https://github.com/olleicua/Testing-Clojure/blob/master/src/test/olleicua/testing_clojure.clj
- Guía de buenas prácticas para escribir pruebas en Clojure: https://betweentwoparens.com/practical-advice-for-writing-clojure-tests