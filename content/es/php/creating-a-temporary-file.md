---
title:                "PHP: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué crear un archivo temporal en PHP

Crear un archivo temporal en PHP puede ser una tarea útil y necesaria en algunos casos, ya sea para almacenar datos temporales o para realizar pruebas en nuestro código. A continuación, te explicamos cómo hacerlo de manera sencilla y eficiente.

## Cómo crear un archivo temporal en PHP

Para crear un archivo temporal en PHP, utilizaremos la función `tempnam()`, que nos permitirá generar un nombre de archivo único y nos devolverá una ruta absoluta al archivo. A continuación, abriremos el archivo utilizando `fopen()` y escribiremos o leeremos los datos que necesitamos. Por último, cerraremos el archivo con `fclose()` y podremos utilizar el archivo temporal creado en nuestro código.

```PHP
<?php
// Crear un archivo temporal
$tmp_file = tempnam("/tmp", "prefix");

// Abrir el archivo en modo escritura
$file = fopen($tmp_file, "w");

// Escribir datos en el archivo
fwrite($file, "Esto es un archivo temporal");

// Cerrar el archivo
fclose($file);

// Podemos utilizar el archivo temporal en nuestro código
echo "Archivo temporal creado en: " . $tmp_file;
```

El código anterior generará un archivo temporal en la carpeta `/tmp` con el prefijo "prefix" seguido de un número aleatorio. Podemos especificar una ruta diferente y un prefijo personalizado según nuestras necesidades.

## Profundizando en la creación de archivos temporales en PHP

La función `tempnam()` también nos permite indicar la carpeta donde queremos crear el archivo temporal y el prefijo que queremos utilizar. Además, podemos utilizar otras funciones como `file_put_contents()` o `file_get_contents()` para escribir o leer datos en nuestro archivo temporal de manera más sencilla.

Es importante tener en cuenta que los archivos temporales se eliminarán automáticamente cuando se cierre el script de PHP. Si necesitas mantener el archivo temporal durante más tiempo, deberás utilizar la función `unlink()` para eliminarlo manualmente.

## Ver también

- [Documentación oficial de PHP sobre la función `tempnam()`](https://www.php.net/manual/es/function.tempnam.php)
- [Artículo sobre la creación de archivos temporales en PHP](https://www.webhostingsecretrevealed.net/blog/php-programming/php-file-handling-tutorial/)
- [Ejemplo práctico de uso de archivos temporales en PHP](https://www.w3schools.com/php/func_filesystem_file_put_contents.asp)