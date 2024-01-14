---
title:                "PHP: Leyendo argumentos de línea de comando"
simple_title:         "Leyendo argumentos de línea de comando"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías leer argumentos de línea de comandos?

Si estás interesado en aprender a programar en PHP, es importante que entiendas cómo leer y utilizar argumentos de línea de comandos. Además, conocer esta función te permitirá automatizar tareas repetitivas y agilizar tu flujo de trabajo.

## Cómo leer argumentos de línea de comandos en PHP

En PHP, puedes leer argumentos de línea de comandos utilizando la función `getopt`. Esta función toma dos argumentos: una cadena con las opciones válidas y un arreglo con los argumentos proporcionados por el usuario. Por ejemplo:

```PHP
<?php
// Declaración de opciones válidas
$opciones = "a:b:c";

// Obtener los argumentos proporcionados por el usuario
$argumentos = getopt($opciones);

// Imprimir el contenido del arreglo $argumentos
print_r($argumentos);
```

Si ejecutamos este código desde la línea de comandos con los argumentos `-a1 -b2 -c3`, obtendremos el siguiente resultado:

```
Array
(
    [a] => 1
    [b] => 2
    [c] => 3
)
```

Cada letra en la cadena de opciones representa una opción válida y el `:` significa que esta opción requiere un argumento. En nuestro ejemplo, la opción `a` requiere un argumento después de ella (en este caso, el número 1).

## Profundizando en la lectura de argumentos de línea de comandos

Además de la función `getopt`, existen otras formas de leer argumentos de línea de comandos en PHP. Por ejemplo, puedes utilizar la función `$_SERVER['argv']` que devuelve un arreglo con todos los argumentos proporcionados por el usuario.

También puedes utilizar la extensión `argv` para obtener los argumentos desde un archivo PHP en lugar de ejecutarlo desde la línea de comandos.

Otra opción es utilizar un framework como Symfony o Laravel, que tienen herramientas integradas para facilitar la lectura y el uso de argumentos de línea de comandos.

## Ver también:

- [Documentación de la función getopt en PHP](https://www.php.net/manual/es/function.getopt.php)
- [Cómo trabajar con argumentos de línea de comandos utilizando PHP](https://desarrolloweb.com/articulos/trabajar-argumentos-php.html)
- [Uso avanzado de argumentos de línea de comandos en PHP](https://blog.learningdollars.com/2019/09/11/advanced-use-of-command-line-arguments-in-php/)