---
title:                "Creando un archivo temporal"
html_title:           "Arduino: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Crear un archivo temporal es el proceso de producir un archivo de corta duración para el almacenamiento de datos transitorios. Los programadores hacen esto para administrar la memoria de manera eficiente y para la manipulación de datos que no necesitan ser persistentes.

## ¿Cómo hacerlo?

Crear un archivo temporal en PHP es bastante simple, utilizando la función `tmpfile()`.

```PHP
<?php
$temporal = tmpfile();

fwrite($temporal, "Hola, Mundo Temporal!");

rewind($temporal);
// Leer y mostrar el contenido 
echo fread($temporal,1024); 

fclose($temporal); // esto elimina el archivo
?>
```

Este código creará un archivo temporal, le escribirá "Hola, Mundo Temporal!", lo mostrará y luego cerrará el archivo (y por lo tanto eliminará el archivo temporal). 

## Inmersión profunda

Historia breve: La función de creación de archivos temporales ha existido desde PHP 4 y ha sido una parte integral de la manipulación de archivos y la administración de memoria en PHP.

Alternativas: En lugar de `tmpfile()`, otra opción es usar `tempnam()` que crea un nombre de archivo único en un directorio específico. Esta es una buena opción si quieres controlar dónde se crea el archivo temporal.

Detalles implementación: `tmpfile()` crea un archivo en el directorio de archivos temporales del sistema, con un nombre de archivo único para evitar conflictos. El archivo se borra automáticamente cuando se cierra (es decir, cuando el script PHP termina o cuando se llama a `fclose()`).

## Ver también

Funciones de manipulación de archivos PHP en el manual de PHP - [PHP: filesystem - Manual](https://www.php.net/manual/es/book.filesystem.php)

PHP tmpfile() Function - [PHP: tmpfile - Manual](https://www.php.net/manual/es/function.tmpfile.php)

Función PHP tempnam() - [PHP: tempnam - Manual](https://www.php.net/manual/es/function.tempnam.php)