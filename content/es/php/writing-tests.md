---
title:                "Escribir pruebas"
html_title:           "PHP: Escribir pruebas"
simple_title:         "Escribir pruebas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir pruebas en PHP?

Escribir pruebas en PHP es fundamental para garantizar la calidad y el funcionamiento adecuado de nuestras aplicaciones. Con las pruebas, podemos detectar y corregir errores antes de que afecten a nuestros usuarios y ahorrar tiempo y esfuerzo en la resolución de problemas imprevistos en producción.

## Cómo escribir pruebas en PHP

El proceso de escribir pruebas en PHP se puede dividir en tres pasos clave: preparación, ejecución y verificación.

**Preparación:** Antes de escribir cualquier prueba, es necesario definir la funcionalidad que queremos probar. Esto nos ayudará a establecer los casos de prueba y los resultados esperados.

**Ejecución:** Para realizar una prueba en PHP, utilizaremos la función `assert()`, la cual espera una expresión booleana como primer parámetro y un mensaje opcional como segundo parámetro para imprimir en caso de que la prueba falle.

**Verificación:** Una vez que se han ejecutado las pruebas, es importante verificar los resultados para asegurarnos de que la funcionalidad probada funciona como se espera. En caso de encontrar errores, podemos corregirlos antes de que afecten a nuestro código en producción.

A continuación, se muestra un ejemplo sencillo de cómo escribir y ejecutar una prueba en PHP utilizando la función `assert()`.

```PHP
<?php

// Definimos la función a probar
function sum($a, $b) {
    return $a + $b;
}

// Realizamos la prueba utilizando assert()
assert(sum(2, 2) == 4, "La suma de 2 y 2 debería ser igual a 4");
```

Si ejecutamos este código y todo está correcto, no se imprimirá nada en pantalla. Sin embargo, si modificamos la función `sum()` para que devuelva un resultado incorrecto, obtendremos el siguiente mensaje de error: `Assertion failed: La suma de 2 y 2 debería ser igual a 4`.

## Profundizando en la escritura de pruebas en PHP

Escribir pruebas en PHP puede ser un proceso más complejo que el ejemplo mostrado anteriormente. Por eso, es importante considerar algunos aspectos durante este proceso.

**Estructurar y organizar las pruebas:** Conforme nuestro código crece, es fundamental estructurar y organizar nuestras pruebas de manera adecuada. Podemos utilizar herramientas como PHPUnit para realizar esto de una manera más eficiente.

**Pruebas unitarias vs pruebas de integración:** Las pruebas unitarias se encargan de probar una unidad de código aislada, como una función o una clase. Por otro lado, las pruebas de integración se enfocan en probar la interacción entre distintas unidades de código. Es importante realizar ambas para tener una cobertura completa de nuestras pruebas.

**Mantener un conjunto de pruebas actualizado:** Conforme nuestro código va cambiando, nuestras pruebas también deberán ser actualizadas para garantizar que sigan funcionando correctamente.

## Ver también

- [PHPUnit Documentation](https://phpunit.readthedocs.io/en/latest/)
- [Pruebas unitarias y de integración en PHP](https://codigofacilito.com/articulos/pruebas-unitarias-integracion-php)
- [Escribiendo pruebas de código en PHP](https://docs.meziantou.net/writing-tests-in-php.html)