---
title:                "Generando números aleatorios"
date:                  2024-01-20T17:49:37.922677-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generando números aleatorios"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
La generación de números aleatorios es simplemente crear valores que no se pueden predecir. Los programadores lo hacen para cosas como juegos, simulaciones y seguridad.

## Cómo hacerlo:
Aquí tienes unos ejemplos para generar números aleatorios en PHP:

```PHP
<?php
// Generar un número aleatorio entre 1 y 100
echo rand(1, 100);
?>
```
Resultado posible: `25`

```PHP
<?php
// Generar un entero aleatorio seguro para criptografía entre 0 y 255
echo random_int(0, 255);
?>
```
Resultado posible: `162`

```PHP
<?php
// Generar un número decimal aleatorio con mt_rand()
echo mt_rand(1, 100) / 10;
?>
```
Resultado posible: `7.3`

## Inmersión Profunda:
Antes, la función `rand()` era la usual para generar números aleatorios, pero no era la mejor para criptografía por no ser suficientemente impredecible. Por eso apareció `random_int()`, que es mejor para criptografía. `mt_rand()` es una versión más rápida y con una mejor distribución que `rand()`, aunque para números que necesitan alta seguridad, sigue siendo mejor `random_int()`. Algunas alternativas fuera de PHP incluyen el uso de librerías específicas de otros lenguajes o dispositivos de hardware dedicados a la generación de aleatoriedad.

## Ver También:
- Documentación oficial de PHP sobre generación de números aleatorios: [PHP: Random Number Generation](https://www.php.net/manual/en/book.math.php)
- Para entender mejor la criptografía y los números aleatorios: Ferguson, Niels; Schneier, Bruce (2003). "Practical Cryptography".
- Si estás interesado en cómo los números aleatorios son usados en juegos, puedes ver: Adams, Ernest; Rollings, Andrew (2006). "Fundamentals of Game Design".