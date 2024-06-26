---
date: 2024-01-26 01:11:05.477065-07:00
description: "C\xF3mo hacerlo: Imagina que tenemos c\xF3digo repetitivo para saludar\
  \ a los usuarios. En cambio, lo vamos a envolver en una funci\xF3n como `greet_user`."
lastmod: '2024-03-13T22:44:59.166322-06:00'
model: gpt-4-1106-preview
summary: "Imagina que tenemos c\xF3digo repetitivo para saludar a los usuarios."
title: "Organizando c\xF3digo en funciones"
weight: 18
---

## Cómo hacerlo:
Imagina que tenemos código repetitivo para saludar a los usuarios. En cambio, lo vamos a envolver en una función como `greet_user`:

```php
function greet_user($name) {
    return "Hola, " . $name . "!";
}

echo greet_user("Alice");
echo greet_user("Bob");
```

Salida:
```
Hola, Alice!
Hola, Bob!
```

Ahora, tienes una herramienta útil que puedes usar en cualquier momento sin reescribir las mismas líneas de código cada vez que quieras decir hola.

## Análisis Profundo
Las funciones han estado en la programación desde los primeros días de FORTRAN en los años '50. Son una piedra angular de la programación estructurada y se tratan de modularidad y aislamiento. ¿Alternativas? Bueno, puedes orientarte a objetos y hablar de clases y métodos, que son funciones con un traje elegante. En cuanto a PHP, los detalles de implementación incluyen especificar valores predeterminados para los parámetros, sugerencias de tipos para las entradas y poder devolver múltiples valores utilizando un array o, a partir de PHP 7.1 en adelante, una lista.

He aquí un giro moderno con declaración de tipos y valores predeterminados:

```php
function add(float $a, float $b = 0.0): float {
    return $a + $b;
}

echo add(1.5);
echo add(1.5, 2.5);
```

PHP 7.4 también introdujo funciones flecha, ayudando a escribir funciones concisas de una sola línea, comúnmente utilizadas en operaciones de arrays:

```php
$numbers = array(1, 2, 3, 4);
$squared = array_map(fn($n) => $n * $n, $numbers);
print_r($squared);
```

Salida:
```
Array
(
    [0] => 1
    [1] => 4
    [2] => 9
    [3] => 16
)
```

## Ver También
- [Manual de PHP sobre Funciones](https://www.php.net/manual/es/functions.user-defined.php)
- [PHP: La Manera Correcta - Funciones](https://phptherightway.com/#functions)
- [Aprender sobre Funciones Flecha en PHP 7.4](https://stitcher.io/blog/short-closures-in-php)
