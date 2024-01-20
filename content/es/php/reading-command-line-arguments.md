---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Los argumentos de línea de comandos en PHP son valores que se pasan al script PHP cuando se ejecuta el programa. Permiten que los scripts sean dinámicos y adaptables, teniendo en cuenta diferentes situaciones de entrada.

## Cómo Se Hace:
En PHP, los argumentos de línea de comandos se leen utilizando la variable predefinida `$argv`. Vamos a crear un script simple que toma un argumento y lo imprime.

```PHP
<?php 
if ($argc > 1) { 
    echo "Hola, " . $argv[1];
} 
?>
```

En el ejemplo anterior, `$argc` es el número total de argumentos pasados, y `$argv` es una matriz que contiene los argumentos. Si lo ejecutas con un argumento, como `php script.php Juan`, obtendrás `Hola, Juan`.

## Profundizando
Los argumentos de la línea de comandos se introdujeron en las primeras versiones de PHP, en consonancia con muchos otros lenguajes de programación que incluyen esta funcionalidad. 

En cuanto a las alternativas, podrías usar la función `getopt()` para interpretar opciones y argumentos de línea de comandos de una forma más sofisticada.

A nivel de implementación, PHP almacena los argumentos de línea de comandos como una matriz global. El primer elemento es siempre el propio script, seguido de los argumentos en el orden en que fueron pasados.

## Ver también
- [Documentación Oficial de Argumentos de Línea de Comandos en PHP](https://www.php.net/manual/es/reserved.variables.argv.php)
- [Cómo trabajar con argumentos de línea de comandos en PHP](https://www.sitepoint.com/php-command-line-arguments)
- [Función getopt()](https://www.php.net/manual/es/function.getopt.php)