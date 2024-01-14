---
title:    "PHP: Creando un archivo temporal"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

Crear un archivo temporal es una práctica común en la programación PHP. Se utiliza para almacenar datos de forma temporal, como variables o resultados de una consulta, y luego se borra una vez que ya no son necesarios. Esto ayuda a mantener la organización y eficiencia de un programa.

## Cómo hacerlo

Hay varias formas de crear un archivo temporal en PHP, pero una de las más sencillas es utilizando la función `tmpfile()`. Esta función crea un archivo temporal con permisos de escritura y lectura, y devuelve un puntero al mismo. A continuación, se pueden utilizar otras funciones como `fwrite()` para escribir datos en el archivo y `fread()` para leerlos.

```
<?php
// Crear un archivo temporal
$archivo_temporal = tmpfile();

// Escribir datos en el archivo
fwrite($archivo_temporal, "Este es un archivo temporal.");

// Leer datos del archivo
echo fread($archivo_temporal, filesize($archivo_temporal));

// Cerrar y borrar el archivo temporal
fclose($archivo_temporal);
?>
```

El resultado de este código sería "Este es un archivo temporal." Además de `tmpfile()`, también se puede utilizar `tempnam()` para crear un archivo temporal con un nombre específico, o simplemente crear un archivo con la extensión `.tmp` en el directorio temporal del sistema.

## Inmersión profunda

Además de las funciones mencionadas anteriormente, PHP también cuenta con la clase `SplFileObject` que proporciona una interfaz orientada a objetos para trabajar con archivos temporales. Esta clase también permite establecer diferentes opciones para el archivo, como su ubicación y permisos.

Otra cosa importante a tener en cuenta al trabajar con archivos temporales es asegurarse de borrarlos una vez que ya no se necesiten. Esto puede hacerse manualmente usando `unlink()` o de forma automática estableciendo la opción `auto_track` en `false` en la función `tmpfile()` o `tempnam()`.

En resumen, crear archivos temporales en PHP es una práctica muy útil para mantener el orden y la eficiencia de un programa, y hay diversas opciones para hacerlo según las necesidades de cada proyecto.

## Ver también

- Documentación oficial de PHP sobre archivos temporales: https://www.php.net/manual/es/book.filesystem.php
- Tutorial de W3Schools sobre archivos temporales en PHP: https://www.w3schools.com/php/php_file_create.asp
- Artículo de SitePoint sobre cómo trabajar con archivos temporales en PHP: https://www.sitepoint.com/php-tmp-file-tips-tricks/