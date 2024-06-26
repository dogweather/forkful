---
date: 2024-01-20 17:56:33.727719-07:00
description: "C\xF3mo Hacerlo: Desde las primeras versiones de PHP, la l\xEDnea de\
  \ comandos ha sido un amigo leal. Aunque muchos lo usan para desarrollo web, PHP\
  \ tambi\xE9n es\u2026"
lastmod: '2024-04-05T22:51:12.910158-06:00'
model: gpt-4-1106-preview
summary: "Desde las primeras versiones de PHP, la l\xEDnea de comandos ha sido un\
  \ amigo leal."
title: "Lectura de argumentos de l\xEDnea de comandos"
weight: 23
---

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
