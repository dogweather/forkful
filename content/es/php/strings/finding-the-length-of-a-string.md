---
title:                "Calculando la longitud de una cadena"
aliases: - /es/php/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:58.445244-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calculando la longitud de una cadena"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Calcular la longitud de una cadena significa saber cuántos caracteres contiene. Los programadores lo hacen para validar entradas, manipular texto y por optimización.

## Cómo hacerlo:
En PHP, usamos `strlen()` para obtener la longitud de una cadena. Aquí unos ejemplos:

```PHP
<?php
$texto = "Hola mundo";
$longitud = strlen($texto);
echo $longitud; // Salida: 10
?>
```

Si tienes emojis o caracteres especiales, necesitarás `mb_strlen()`:

```PHP
<?php
$emoji = "🚀";
echo strlen($emoji); // Salida posible: 4 (o incorrecta)
echo mb_strlen($emoji, 'UTF-8'); // Salida correcta: 1
?>
```

## Inmersión Profunda:
La función `strlen()` existe desde los primeros días de PHP. Calcula la longitud de una cadena basándose en bytes, lo que está bien para textos en ASCII pero falla con UTF-8 y otros caracteres multibyte. Ahí entra `mb_strlen()`, parte de la extensión "Multibyte String" de PHP. Esta función considera la codificación actual y cuenta los caracteres correctamente, incluso para emojis y caracteres internacionales. Una alternativa más antigua es `iconv_strlen()`, pero `mb_strlen()` es más popular ahora.

Nota técnica: la complejidad computacional de `strlen()` es O(1), porque las cadenas en PHP almacenan su longitud. Para `mb_strlen()`, depende de la codificación y puede ser hasta O(n) en la peor de las casos, donde n es el número de caracteres.

## Ver También:
- [Documentación de `strlen()`](https://www.php.net/manual/es/function.strlen.php)
- [Documentación de `mb_strlen()`](https://www.php.net/manual/es/function.mb-strlen.php)
- [PHP Multibyte String](https://www.php.net/manual/es/book.mbstring.php)
