---
title:                "Clojure: Escribiendo pruebas"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué

El desarrollo de software puede ser una tarea compleja y a menudo se requiere de mucho tiempo y esfuerzo para crear un producto de alta calidad. Y aunque puede ser tentador omitir el proceso de escribir pruebas, es crucial para garantizar que nuestro código funciona correctamente y continúa funcionando correctamente a medida que se hacen cambios. Las pruebas nos permiten detectar y corregir errores de manera efectiva, lo que nos ahorra tiempo y dinero a largo plazo.

## Cómo hacerlo

¡Escribir pruebas en Clojure es más fácil de lo que piensas! Primero, necesitamos tener instalado Clojure y una librería de pruebas como Midje o Expectations. Luego, simplemente podemos crear una función de prueba dentro de nuestro código utilizando la sintaxis `fact` en Midje o `expect` en Expectations, y proporcionar un valor de entrada y una salida esperada. Por ejemplo, en Midje podemos escribir lo siguiente:

```Clojure
(fact "Realizar una suma"
     (+ 2 3) => 5)
```

Y cuando ejecutamos nuestras pruebas, si el resultado no coincide con el esperado, recibiremos un mensaje de error que nos ayudará a identificar y corregir el problema. Además, también podemos escribir pruebas para verificar otras funciones o comportamientos, como errores esperados o tipos de datos retornados.

## Profundizando

Escribir pruebas también nos permite tener una mayor comprensión de nuestro código y de cómo interactúan las diferentes partes de nuestro programa. Además, nos permite probar diferentes casos y condiciones para asegurarnos de que nuestro código sea robusto y maneje todas las situaciones posibles.

También es importante tener en cuenta que escribir pruebas significa que nuestro código está documentado. Las pruebas pueden ser una forma de documentación para otros desarrolladores que trabajen en nuestro proyecto, ya que pueden ver cómo se supone que funcionan nuestras funciones y cómo deben ser utilizadas.

## Ver también

- Guía de pruebas en Clojure: https://clojure.org/guides/learn/testing
- Documentación de Midje: https://github.com/marick/Midje/wiki/Basics 
- Documentación de Expectations: http://jayfields.com/expectations/