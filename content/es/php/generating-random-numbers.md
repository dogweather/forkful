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

## ¡Generando Números Aleatorios en PHP!

En la programación, a menudo es necesario generar números aleatorios para simular situaciones impredecibles o tomar decisiones al azar. Esto se puede lograr de manera sencilla en PHP utilizando la función ```rand()``` o ```mt_rand()```.

## ¿Qué y Por Qué?

Generar números aleatorios se trata de obtener valores numéricos de forma aleatoria, sin ningún patrón predecible. Los programadores utilizan esto para simular eventos como tirar un dado, seleccionar un ganador al azar, o simplemente para agregar un poco de variabilidad en su código.

## Cómo hacerlo:

Para utilizar la función ```rand()``` en PHP, debes especificar un rango de números entre los cuales quieres generar el número aleatorio. Por ejemplo, si quieres un número aleatorio entre 1 y 10, usarías el código ```rand(1, 10)```. La función devolverá un número aleatorio cada vez que se ejecute el código.

Para una funcionalidad más avanzada, puedes utilizar la función ```mt_rand()```, que utiliza un algoritmo de generación de números aleatorios más complejo y produce resultados más aleatorios. Su uso es similar a la función ```rand()```, solo que esta acepta solo un parámetro, el número más alto del rango.

Un ejemplo de código que utilice ambas funciones podría verse así:

```php
//Genera un número aleatorio entre 1 y 10
$rand_num = rand(1, 10);
echo "El número aleatorio es: " . $rand_num;

//Genera un número aleatorio entre 1 y 100
$mt_rand_num = mt_rand(1, 100);
echo "El número aleatorio es: " . $mt_rand_num;
```

La salida de este código sería algo similar a:

```
El número aleatorio es: 7
El número aleatorio es: 91
```

## Más Detalles:

La función ```rand()``` en PHP utiliza el algoritmo Park-Miller para generar los números aleatorios. Este algoritmo es conocido por tener patrones predecibles en ciertos intervalos, por lo que si necesitas una mayor aleatoriedad, es mejor utilizar la función ```mt_rand()```.

Además, PHP también ofrece la función ```random_int()```, que genera números aleatorios criptográficamente seguros. Esto es importante si estás trabajando con información sensible, ya que garantiza que los números aleatorios no se puedan predecir y se utilizan para fines maliciosos.

## En Resumen:

Generar números aleatorios en PHP es esencial para agregar variabilidad en tus programas y simular comportamientos impredecibles. Puedes hacerlo fácilmente con las funciones ```rand()``` y ```mt_rand()```, pero si necesitas una mayor aleatoriedad y seguridad, es recomendable utilizar la función ```random_int()```.

## Consulta también:

Para obtener más información sobre la generación de números aleatorios en PHP, puedes consultar la documentación oficial en [php.net/manual/es/function.rand.php](php.net/manual/es/function.rand.php) y [php.net/manual/es/function.mt-rand.php](php.net/manual/es/function.mt-rand.php). También puedes explorar otras alternativas como las extensiones [random_compat](https://github.com/paragonie/random_compat) y [random-lib](https://github.com/ircmaxell/random-lib). ¡Diviértete explorando y creando con números aleatorios en PHP!