---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "PHP: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Qué es y Por Qué?
La lectura de argumentos de línea de comandos es una parte común de programación en PHP. Es la forma en que un programa recibe información de la línea de comandos (por ejemplo, al ejecutarse desde la terminal). Los programadores lo hacen para poder personalizar y controlar cómo sus programas se ejecutan y procesan datos.

Cómo Hacerlo:
Hay varias formas de leer argumentos de línea de comandos en PHP, pero la más común es usando la función incorporada "getopt()". Esta función toma dos parámetros: una cadena que especifica qué opciones se deben buscar y un arreglo que contiene los valores correspondientes. Aquí hay un ejemplo:

```PHP
$options = getopt("a:b:c:");
var_dump($options);
```

Salida:

```php
php script.php -a optionA -b optionB -c optionC
array(3) {
  ["a"] => "optionA"
  ["b"] => "optionB"
  ["c"] => "optionC"
}
```

Otra forma de leer argumentos de línea de comandos es usando la variable global "$argv", que contiene todos los argumentos pasados al programa en un arreglo. Aquí hay un ejemplo:

```PHP
var_dump($argv);
```

Salida:

```php
php script.php hello world
array(3) {
  [0] => "script.php" 
  [1] => "hello"
  [2] => "world"
}
```

Profundizando:
La lectura de argumentos de línea de comandos es una técnica común no solo en PHP, sino en otros lenguajes de programación como Python y Java. Se utiliza principalmente para hacer que los programas sean más dinámicos y fáciles de usar, ya que los usuarios pueden personalizar su funcionamiento mediante la especificación de argumentos. Algunas alternativas a la función "getopt()" incluyen "argv" en PHP, "sys.getargv" en Python y "getopt()" en Java.

En términos de implementación, la función "getopt()" en PHP utiliza la biblioteca C "getopt_long()", que es una función estándar en muchos sistemas operativos basados ​​en Unix. Esta función procesa una cadena de opciones y busca en el arreglo de argumentos globales correspondiente.

Ver también:
- Documentación oficial de PHP sobre lectura de argumentos de línea de comandos: https://www.php.net/manual/en/function.getopt.php
- Ejemplos de uso de "getopt()": https://www.geeksforgeeks.org/php-getopt-function/