---
date: 2024-01-20 17:56:33.727719-07:00
description: "Leer argumentos de la l\xEDnea de comandos en PHP permite que tus scripts\
  \ entiendan e interpreten datos de entrada proporcionados al ejecutarse. \xBFPor\
  \ qu\xE9? Es\u2026"
lastmod: '2024-03-13T22:44:59.175825-06:00'
model: gpt-4-1106-preview
summary: "Leer argumentos de la l\xEDnea de comandos en PHP permite que tus scripts\
  \ entiendan e interpreten datos de entrada proporcionados al ejecutarse."
title: "Lectura de argumentos de l\xEDnea de comandos"
weight: 23
---

## Qué y Por Qué?
Leer argumentos de la línea de comandos en PHP permite que tus scripts entiendan e interpreten datos de entrada proporcionados al ejecutarse. ¿Por qué? Es simple: personalización al momento. Dependiendo de los argumentos, el comportamiento del script puede variar sin tener que modificar el código fuente.

## Cómo Hacerlo:
```PHP
<?php
// Comprobar que existen argumentos
if ($argc > 1) {
    // Recorre todos los argumentos (excepto $argv[0] que es el nombre del script)
    for ($i = 1; $i < $argc; $i++) {
        echo "Argumento $i: " . $argv[$i] . "\n";
    }
} else {
    echo "No se proporcionaron argumentos.\n";
}
?>
```
Ejecuta tu script así:
```
php tu_script.php primerArgumento segundoArgumento
```
Salida esperada:
```
Argumento 1: primerArgumento
Argumento 2: segundoArgumento
```

## Análisis Profundo:
Desde las primeras versiones de PHP, la línea de comandos ha sido un amigo leal. Aunque muchos lo usan para desarrollo web, PHP también es poderoso para scripts de consola. `$argc` y `$argv` son variables automáticas: `$argc` te dice cuántos argumentos se pasaron, y `$argv` es un array que los contiene. Hay alternativas modernas, como la extensión `getopt()`, que es más sofisticada, o `Symfony Console` para aplicaciones de consola más robustas. Pero para la esencia pura de PHP de línea de comandos, `$argv` y `$argc` son los caballos de batalla.

## Ver También:
- [Documentación oficial de PHP en Argumentos de línea de comandos](https://www.php.net/manual/es/features.commandline.php)
- [Symfony Console Component](https://symfony.com/doc/current/components/console.html)
- [Composer](https://getcomposer.org/) - Gestor de dependencias en PHP, útil para instalar herramientas como Symfony Console
