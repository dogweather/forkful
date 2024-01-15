---
title:                "Leyendo argumentos de línea de comandos"
html_title:           "PHP: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

Leer argumentos de línea de comandos puede ser útil en situaciones donde se necesita interactuar con un programa en modo texto o ejecutar scripts con diferentes parámetros. También puede ser una forma rápida de personalizar la ejecución de un programa sin tener que modificar su código.

## Cómo hacerlo

Para leer argumentos de línea de comandos en PHP, utilizamos la función `getopt()` que toma tres parámetros: una cadena con las opciones permitidas, un array con los argumentos de línea de comandos y un array opcional con opciones adicionales.

```PHP
<?php
$options = getopt("fha:", ["file:", "help", "age:"]);

if (array_key_exists("f", $options) || array_key_exists("file", $options)) {
    echo "Se especificó el archivo: " . $options["f"] ?? $options["file"];
} else if (array_key_exists("h", $options) || array_key_exists("help", $options)) {
    echo "Se pidió ayuda";
} else if (array_key_exists("a", $options) || array_key_exists("age", $options)) {
    echo "Se especificó la edad: " . $options["a"] ?? $options["age"];
} else {
    echo "No se especificaron opciones";
}
```

Ejemplo de uso:

```bash
php script.php -f archivo.txt -a 25
Se especificó el archivo: archivo.txt
Se especificó la edad: 25
```

```bash
php script.php --file archivo.txt --help
Se especificó el archivo: archivo.txt
Se pidió ayuda
```

```bash
php script.php -h -a 30
Se pidió ayuda
Se especificó la edad: 30
```

## Profundizando

La función `getopt()` devuelve un array asociativo con las opciones especificadas y sus respectivos valores. Si una opción requiere un valor, se puede utilizar `:` después del caracter correspondiente (por ejemplo, "a:" en vez de "a").

Además, podemos acceder a los argumentos restantes utilizando la variable global `$argv` y su función `shift()` para remover el primer elemento que contiene el nombre del script.

Si necesitamos validar los argumentos o realizar algún tipo de manejo de errores, se puede utilizar la función `getopt()` dentro de un bucle y realizar las acciones correspondientes en cada caso.

## Ver también

- Documentación oficial de PHP sobre [la función `getopt()`](https://www.php.net/manual/es/function.getopt.php)
- [Argumentos de línea de comandos en PHP](https://parzibyte.me/blog/2019/08/20/argumentos-linea-comandos-php/) en el blog de Parzibyte