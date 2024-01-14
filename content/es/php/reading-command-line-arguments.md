---
title:                "PHP: Leyendo argumentos de línea de comandos"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías leer argumentos de línea de comandos en tus programas PHP?

Incluir la funcionalidad de leer argumentos de línea de comandos en tus programas PHP te permite interactuar con ellos de manera más dinámica y personalizada. Por ejemplo, podrías crear un programa que acepte la entrada de un archivo a través de la línea de comandos para procesarlo de manera específica.

## ¿Cómo hacerlo?

```PHP
<?php

// Lee el primer argumento después del nombre del archivo
$archivo = $argv[1];

// Imprime el nombre del archivo
echo "Archivo ingresado: " . $archivo . "\n";

// Lee el segundo argumento después del nombre del archivo
$opcion = $argv[2];

// Imprime la opción elegida
echo "Opción seleccionada: " . $opcion . "\n";
```

Si ejecutas este programa desde la línea de comandos con los argumentos "archivo.txt" y "comprimir", el resultado sería el siguiente:

```
Archivo ingresado: archivo.txt
Opción seleccionada: comprimir
```

¡Así de sencillo es leer y utilizar los argumentos de línea de comandos en PHP! También puedes utilizar bucles y condicionales para manejar diferentes situaciones con diferentes argumentos.

## Adentrándonos más en la lectura de argumentos de línea de comandos

Además de leer argumentos individuales, también es posible leer toda la cadena de argumentos de una sola vez utilizando la función `implode()`. De igual manera, puedes utilizar la función `getopt()` para leer opciones con valores específicos en una cadena de argumentos.

Puedes explorar más a fondo estas funciones y otras formas de leer y utilizar los argumentos de línea de comandos en la [documentación oficial de PHP](https://www.php.net/manual/es/features.commandline.php).

## Ver también

- [Cómo crear un programa simple en PHP](https://www.ejemplophp.com/crear-programa-simple-php/)
- [Guía para principiantes de PHP](https://www.cualquieraprendo.com/php.php)
- [Documentación de PHP sobre argumentos de línea de comandos](https://www.php.net/manual/es/features.commandline.php)