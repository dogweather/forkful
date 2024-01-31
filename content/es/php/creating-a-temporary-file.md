---
title:                "Creando un archivo temporal"
date:                  2024-01-20T17:40:51.548537-07:00
model:                 gpt-4-1106-preview
simple_title:         "Creando un archivo temporal"

category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Qué & Por Qué?
Crear un archivo temporal es simplemente generar un fichero que solo necesitas durante la ejecución de tu script PHP. Los programadores lo hacen para no dejar rastro en el sistema de archivos permanente o para manejar datos que solo son relevantes de forma transitoria.

## Cómo hacerlo:
Generar un archivo temporal en PHP es pan comido con la función `tmpfile()`:

```PHP
<?php
$temporal = tmpfile();
fwrite($temporal, "Guardando datos temporalmente aquí...");
rewind($temporal); // Vuelve al inicio del archivo

// Leer y mostrar el contenido del archivo temporal
echo fread($temporal, 1024);

// El archivo se eliminará automáticamente al cerrarse
fclose($temporal);
?>
```

Si ejecutas esto verás el output:
```
Guardando datos temporalmente aquí...
```

## Inmersión Profunda:
Antes de `tmpfile()`, guardar datos sin afectar el sistema de archivos era un baile entre crear y borrar archivos, propenso a errores. Además, `tempnam()` y `sys_get_temp_dir()` son alternativas que permiten más control, creando un archivo temporal con nombre pero que requiere eliminación manual.

La función `tmpfile()` de PHP es una solución segura y fácil, creando un archivo con un nombre único en el directorio temporal del sistema, y lo mejor, se elimina automáticamente al cerrar el recurso (o cuando el script PHP acaba).

## Ver También:
- Documentación oficial de PHP sobre `tmpfile()`: https://www.php.net/manual/es/function.tmpfile.php
- Función `tempnam()`: https://www.php.net/manual/es/function.tempnam.php
- Función `sys_get_temp_dir()`: https://www.php.net/manual/es/function.sys-get-temp-dir.php
