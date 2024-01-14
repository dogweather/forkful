---
title:                "PHP: Generando números aleatorios"
programming_language: "PHP"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué generar números aleatorios es importante para la programación

Generar números aleatorios es una habilidad esencial en la programación ya que permite crear funcionalidades como juegos, gestión de contraseñas, pruebas de software y muchas más. Además, es una forma efectiva de agregar emoción y aleatoriedad a cualquier aplicación o programa.

## Cómo generar números aleatorios en PHP

Para generar números aleatorios en PHP, podemos utilizar la función `rand (inicio, final)` donde `inicio` y `final` son los números que deseamos que la función genere en un rango específico. Por ejemplo:

```PHP
<?php
echo "Número aleatorio entre 1 y 10: " . rand(1, 10);
// Ejemplo de salida: Número aleatorio entre 1 y 10: 7
?>
```

También podemos utilizar la función `mt_rand (inicio, final)` que utiliza un mejor algoritmo para generar números aleatorios. A continuación, mostraremos cómo utilizarla:

```PHP
<?php
echo "Número aleatorio entre 1 y 10: " . mt_rand(1, 10);
// Ejemplo de salida: Número aleatorio entre 1 y 10: 9
?>
```

Como podemos observar, ambas funciones aceptan un rango de números y generan un número aleatorio dentro de ese rango. Sin embargo, es importante tener en cuenta que la función `mt_rand` es más eficiente y recomendada para generar números aleatorios en PHP.

## Profundizando en la generación de números aleatorios

Existen diferentes formas de generar números aleatorios en PHP, como la función `rand` y `mt_rand`, pero también podemos utilizar la función `shuffle` para ordenar de forma aleatoria un array de números. Veamos un ejemplo:

```PHP
<?php
$numeros = array(1, 2, 3, 4, 5);
shuffle($numeros);
foreach ($numeros as $numero) {
    echo $numero . " ";
}
// Ejemplo de salida: 2 4 3 1 5
?>
```

También podemos utilizar la función `mt_srand` para establecer una semilla y así tener una secuencia de números aleatorios específica.

```PHP
<?php
mt_srand(1000);
echo "Número aleatorio: " . mt_rand(1, 10);
// Ejemplo de salida: Número aleatorio: 3
mt_srand(1000);
echo "Número aleatorio: " . mt_rand(1, 10);
// Ejemplo de salida: Número aleatorio: 3
?>
```

En este caso, ambas llamadas a `mt_rand` generarán el mismo número debido a que se estableció una semilla específica. Esto puede ser útil para generar números aleatorios en una secuencia determinada durante las pruebas de un programa.

## Ver también

- [Documentación de PHP sobre la función `rand`](https://www.php.net/manual/es/function.rand.php)
- [Documentación de PHP sobre la función `mt_rand`](https://www.php.net/manual/es/function.mt-rand.php)
- [Ejemplos prácticos de generación de números aleatorios en PHP](https://stackabuse.com/using-phps-rand-function-for-random-number-generation/)
- [Tutorial de YouTube: Generar números aleatorios en PHP](https://www.youtube.com/watch?v=FtyJbMTZiLk)