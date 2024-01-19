---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

La generación de números aleatorios es la creación de números en un proceso no predecible ni repetible. Los programadores la utilizan para varias funciones como la generación de identificadores únicos, datos de prueba y la creación de comportamientos impredecibles en los juegos.

## Cómo hacerlo:

Existen varias formas de generar números aleatorios en PHP. Aquí te mostramos dos formas comunes usando la función `rand()`. 

```PHP
<?php
// Genera un número aleatorio entre 0 y 32767
$numeroAleatorio = rand();
echo $numeroAleatorio;
?>
```

```PHP
<?php
// Genera un número aleatorio entre 1 y 10
$numeroAleatorio = rand(1, 10);
echo $numeroAleatorio;
?>
```

## Inmersión Profunda:

(1) **Contexto histórico** 

PHP ha tenido la función rand() desde su versión 4. La función `rand()` utiliza el algoritmo 'Mersenne Twister', introducido en PHP 7.1, que ofrece un mejor generador de números pseudo-aleatorios.

(2) **Alternativas** 

Además de `rand()`, también puedes usar `mt_rand()`, `random_int()`, entre otras, dependiendo de tus necesidades. `mt_rand()` es cuatro veces más rápido pero va a ser menos criptográficamente seguro que `random_int()`.

```PHP
<?php
// Usando mt_rand()
$numeroAleatorio = mt_rand(1, 10);
echo $numeroAleatorio;

// Usando random_int()
$numeroAleatorio = random_int(1, 10);
echo $numeroAleatorio;
?>
```

(3) **Detalles de implementación** 

Para asegurar que tus números aleatorios son verdaderamente impredecibles, considera sembrar el generador con `mt_srand()`. 

```PHP
<?php
// Sembrando el generador con el tiempo actual
mt_srand(time());

$numeroAleatorio = mt_rand();
echo $numeroAleatorio;
?>
```

## Ver También:

- [PHP Manual: rand()](https://www.php.net/manual/es/function.rand.php)
- [PHP Manual: mt_rand()](https://www.php.net/manual/es/function.mt-rand.php)
- [PHP Manual: random_int()](https://www.php.net/manual/es/function.random-int.php)
- [PHP Manual: mt_srand()](https://www.php.net/manual/es/function.mt-srand.php)