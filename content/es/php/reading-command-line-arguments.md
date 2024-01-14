---
title:    "PHP: Leyendo argumentos de línea de comandos"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## ¿Por qué deberías leer argumentos de línea de comandos?

Si estás aprendiendo programación en PHP, es importante conocer cómo leer y utilizar argumentos de línea de comandos. Estos argumentos son útiles para pasar información a través de la línea de comandos y pueden ser usados en diversas situaciones, como la ejecución de scripts desde una terminal o la creación de scripts de automatización. En esta publicación, te mostraremos cómo leer argumentos de línea de comandos en PHP y cómo pueden ser útiles en tu trabajo diario como programador.

## Cómo hacerlo

Para leer argumentos de línea de comandos en PHP, se utilizan las variables predefinidas `$argv` y `$argc`. `$argv` es un arreglo que contiene los argumentos pasados a través de la línea de comandos, mientras que `$argc` contiene el número total de argumentos. Veamos un ejemplo simple:

```PHP
<?php
// recibe 2 argumentos desde la línea de comandos
// php script.php argumento1 argumento2

echo "El primer argumento es: " . $argv[1]; // imprime "argumento1"
echo "El segundo argumento es: " . $argv[2]; // imprime "argumento2"

```

Puedes ver cómo podemos acceder a los argumentos a través del arreglo `$argv` y utilizarlos en nuestras operaciones. También puedes utilizar un bucle `foreach` para recorrer todos los argumentos pasados. Además, puedes utilizar funciones como `count()` para verificar el número total de argumentos antes de realizar alguna acción.

Otro ejemplo útil es la utilización de argumentos en variables de entorno. En este caso, pasamos la información a través de la línea de comandos y luego la asignamos a una variable para su uso en nuestro script. Veamos un ejemplo:

```PHP
<?php
// ejecutamos desde la terminal
// NOMBRE="Juan" php script.php

// asignamos el valor del argumento a una variable de entorno
$nombre = $_ENV["NOMBRE"];

echo "¡Hola" . $nombre . "! Bienvenido a mi script.";

```

Este es un ejemplo sencillo, pero puedes utilizar esta técnica para pasar nombres de archivos, rutas de archivos o cualquier otra información necesaria para tu script.

## Inmersión Profunda

Si quieres profundizar en la lectura de argumentos de línea de comandos en PHP, una de las funciones más útiles que puedes utilizar es `getopt()`. Esta función te permite obtener argumentos con nombres específicos, lo que hace que tu código sea más legible y fácil de usar. También puedes especificar opciones y argumentos requeridos o permitir que algunos sean opcionales. Puedes encontrar más información sobre la función `getopt()` en la [documentación oficial de PHP](https://www.php.net/manual/es/function.getopt.php).

## Ver También

- [Documentación oficial de PHP sobre argumentos de línea de comandos](https://www.php.net/manual/es/features.commandline.php)
- [Tutorial de DigitalOcean sobre argumentos de línea de comandos en PHP](https://www.digitalocean.com/community/tutorials/how-to-read-command-line-arguments-in-php)
- [Video tutorial sobre cómo utilizar argumentos de línea de comandos en PHP](https://www.youtube.com/watch?v=xq-wO8X_6n0)

¡Hasta aquí llega nuestra guía sobre cómo leer argumentos de línea de comandos en PHP! Esperamos que te haya sido útil y que puedas utilizar esta técnica en tus proyectos futuros. Recuerda que la práctica hace al maestro, así que no dudes en experimentar y descubrir nuevas formas de utilizar argumentos de línea de comandos en tu código. ¡Feliz programación!