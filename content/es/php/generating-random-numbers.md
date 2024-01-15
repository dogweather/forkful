---
title:                "Generación de números aleatorios"
html_title:           "PHP: Generación de números aleatorios"
simple_title:         "Generación de números aleatorios"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has necesitado generar un número aleatorio en tus programas de PHP? Ya sea para crear contraseñas seguras, realizar pruebas o simplemente para agregar un elemento de aleatoriedad, la generación de números aleatorios es una habilidad importante en la programación. En este artículo, aprenderás cómo generar fácilmente números aleatorios en PHP y algunas consideraciones importantes a tener en cuenta.

## Cómo

La generación de números aleatorios en PHP es muy sencilla gracias a la función `rand()`. Esta función toma dos parámetros, el valor mínimo y el máximo, y retorna un número aleatorio dentro de ese rango. Por ejemplo, si queremos generar un número aleatorio entre 1 y 10, nuestro código se vería así:

```PHP
$numero_aleatorio = rand(1, 10);
echo "Número aleatorio: " . $numero_aleatorio;

// Output:
// Número aleatorio: 7
```

También podemos generar números aleatorios con decimales utilizando la función `mt_rand()`. Esta función toma dos parámetros, el valor mínimo y el máximo, pero en lugar de retornar un número entero, retorna un número con decimales. Veamos un ejemplo:

```PHP
$numero_aleatorio = mt_rand(0, 1) / 10;
echo "Número aleatorio con decimales: " . $numero_aleatorio;

// Output:
// Número aleatorio con decimales: 0.4
```

Además de `rand()` y `mt_rand()`, también podemos utilizar la función `random_int()`, que es más segura y genera números criptográficamente seguros. Esta función toma dos parámetros, el valor mínimo y el máximo, y retorna un número entero. Por ejemplo:

```PHP
$numero_aleatorio = random_int(10, 20);
echo "Número aleatorio seguro: " . $numero_aleatorio;

// Output:
// Número aleatorio seguro: 13
```

Es importante tener en cuenta que la generación de números aleatorios depende del generador de números aleatorios (RNG) predeterminado de PHP. Si queremos utilizar un generador diferente, podemos utilizar la función `srand()` para establecer un nuevo generador antes de usar `rand()` o `mt_rand()`. También podemos utilizar las constantes `RAND_PHP` y `RAND_MT` para especificar qué generador queremos utilizar.

## Profundizando

Aunque la generación de números aleatorios en PHP es bastante sencilla, es importante tener en cuenta algunas consideraciones al utilizar esta función. En primer lugar, es importante saber que estos números no son realmente "aleatorios" en el sentido verdadero de la palabra. En lugar de eso, son números pseudoaleatorios que se generan utilizando un algoritmo matemático. Por lo tanto, si utilizamos un generador de números aleatorios diferente en el mismo rango, obtendremos el mismo resultado.

Además, es importante tener en cuenta que los números generados con `rand()` y `mt_rand()` no son criptográficamente seguros. Si necesitas generar números seguros para contraseñas o información confidencial, es mejor utilizar la función `random_int()` o un generador de números aleatorios externo como Random.org.

Por último, es importante mencionar que la mayoría de los generadores de números aleatorios dependen de un "semilla" que determina los números que serán generados. Si utilizamos la misma semilla, obtendremos los mismos números en el mismo orden. Por defecto, PHP utiliza la hora actual como semilla, pero podemos especificar una semilla personalizada con la función `srand()`.

## Ver también

- [Documentación de PHP sobre rand()](https://www.php.net/manual/en/function.rand.php)
- [Documentación de PHP sobre mt_rand()](https://www.php.net/manual/en/function.mt-rand.php)
- [Documentación de PHP sobre random_int()](https://www.php.net/manual/en/function.random-int.php)