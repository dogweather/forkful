---
title:    "PHP: Generando números aleatorios"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por qué generar números aleatorios en programación

Generar números aleatorios es una habilidad básica en programación que puede tener varias aplicaciones. Desde juegos hasta simulaciones, pasando por pruebas de algoritmos, la generación de números aleatorios es esencial para crear programas dinámicos y variados.

## Cómo generar números aleatorios en PHP

En PHP, podemos generar números aleatorios utilizando la función `rand()`, que acepta dos parámetros opcionales: el número más bajo y el más alto que queremos que se incluyan en la generación de números aleatorios. Aquí hay un ejemplo de cómo generar 10 números aleatorios entre 1 y 100:

```PHP
<?php
for ($i = 0; $i < 10; $i++) {
  $random_number = rand(1, 100);
  echo $random_number . "\n";
}
```

Resultado:

```
37
83
12
55
94
26
68
100
49
1
```

También podemos utilizar la función `mt_rand()` para generar números aleatorios de forma más eficiente y con mayor precisión. Mientras que `rand()` utiliza el generador de números aleatorios estándar de PHP, `mt_rand()` utiliza el generador de números aleatorios Mersenne Twister, que es más rápido y produce números más uniformemente distribuidos.

## Profundizando en la generación de números aleatorios

Las secuencias de números aleatorios generadas por las funciones anteriores no son realmente aleatorias, sino que siguen un patrón predecible. Para crear secuencias verdaderamente aleatorias, necesitaremos utilizar semillas, que son valores iniciales que alimentamos al generador de números aleatorios. En PHP, podemos proporcionar una semilla a través de la función `srand()`, que debe ser llamada antes de utilizar `rand()` o `mt_rand()`.

Otra forma de generar números aleatorios es utilizando la función `shuffle()`, que mezcla los elementos de una matriz de forma aleatoria. Esta función es útil para casos en los que necesitamos generar una lista ordenada de elementos en un orden aleatorio, como en un juego de cartas.

## Ver también

- [Documentación oficial de PHP sobre la generación de números aleatorios](https://www.php.net/manual/es/function.rand.php)
- [Mersenne Twister en Wikipedia](https://es.wikipedia.org/wiki/Mersenne_Twister)
- [Función srand() en PHP](https://www.php.net/manual/es/function.srand.php)
- [Función shuffle() en PHP](https://www.php.net/manual/es/function.shuffle.php)