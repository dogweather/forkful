---
title:    "PHP: Generando números aleatorios"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué Generar Números Aleatorios es Importante en Programación PHP

Los números aleatorios son una herramienta indispensable en la programación PHP ya que permiten crear aplicaciones dinámicas e interactivas. Además, son útiles para realizar pruebas de rendimiento y generar datos de prueba.

## Cómo Generar Números Aleatorios en PHP

Para generar números aleatorios en PHP, se puede utilizar la función `rand()` que devuelve un número entero aleatorio entre un valor mínimo y máximo especificado. Por ejemplo:

```PHP
$numero = rand(1, 10);
echo "El número aleatorio generado es: " . $numero;

// Output: El número aleatorio generado es: 7
```

También se puede utilizar la función `mt_rand()` que utiliza un algoritmo de mejor calidad y por lo tanto, genera números más aleatorios. Esta función también devuelve un número entero aleatorio entre un valor mínimo y máximo especificado. Por ejemplo:

```PHP
$numero = mt_rand(100, 1000);
echo "El número aleatorio generado es: " . $numero;

// Output: El número aleatorio generado es: 513
```

Otra opción es generar números aleatorios flotantes utilizando la función `lcg_value()`, que devuelve un número aleatorio entre 0 y 1. Por ejemplo:

```PHP
$numero = lcg_value();
echo "El número aleatorio generado es: " . $numero;

// Output: El número aleatorio generado es: 0.54226893411816
```

## Profundizando en la Generación de Números Aleatorios en PHP

Aunque las funciones mencionadas anteriormente son las más utilizadas para generar números aleatorios en PHP, también existe la función `random_int()` que permite generar números criptográficamente seguros. Esto significa que los números generados son impredecibles y no pueden ser adivinados fácilmente por alguien con intenciones maliciosas.

Otra técnica para generar números aleatorios en PHP es utilizando la función `str_shuffle()` para mezclar una cadena de caracteres y luego obtener un número entero a partir de esa cadena. Por ejemplo:

```PHP
$numero = (int) str_shuffle("1234567890");
echo "El número aleatorio generado es: " . $numero;

// Output: El número aleatorio generado es: 8435167920
```

## Ver también

- [Documentación oficial de PHP sobre la función rand()](https://www.php.net/manual/es/function.rand.php)
- [Documentación oficial de PHP sobre la función mt_rand()](https://www.php.net/manual/es/function.mt-rand.php)
- [Documentación oficial de PHP sobre la función lcg_value()](https://www.php.net/manual/es/function.lcg-value.php)
- [Documentación oficial de PHP sobre la función random_int()](https://www.php.net/manual/es/function.random-int.php)
- [Documentación oficial de PHP sobre la función str_shuffle()](https://www.php.net/manual/es/function.str-shuffle.php)