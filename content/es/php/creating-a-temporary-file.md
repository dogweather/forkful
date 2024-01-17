---
title:                "Creando un archivo temporal"
html_title:           "PHP: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Crear un archivo temporal en PHP es la acción de generar un archivo en el sistema que se utilizará temporalmente para almacenar información. Los programadores pueden necesitar crear estos archivos para realizar operaciones de lectura y escritura de datos de manera más eficiente, ya que los archivos temporales son más sencillos y rápidos de manejar que otros tipos de almacenamiento.

## ¿Cómo hacerlo?

```PHP
$tempFile = tempnam(sys_get_temp_dir(), 'prefix_'); // Crear un archivo temporal con prefijo
echo $tempFile; // Imprimir la ubicación del archivo temporal
```

La función `tempnam()` de PHP toma dos parámetros: la ruta del directorio temporal del sistema y un prefijo (opcional). Al ejecutarla, se creará un archivo con un nombre aleatorio y se devolverá su ubicación, que se puede almacenar en una variable para su uso posterior. Este archivo se eliminará automáticamente cuando se cierre el script.

## Profundizando

Esta práctica se remonta a los primeros días de la informática y aún se utiliza en la programación moderna. Los archivos temporales también son una forma segura de compartir datos entre diferentes aplicaciones y procesos, ya que tienen permisos de acceso restringidos. Además, en lugar de crear un archivo temporal, los programadores también pueden usar funciones como `tempfile()` o `fopen()` para generar un recurso temporal en lugar de un archivo físico.

## Ver también

- [Documentación oficial de PHP sobre creación de archivos temporales](https://www.php.net/manual/en/function.tempnam.php)
- [Uso de archivos temporales en programación](https://www.programmersought.com/article/28058450665/) (en inglés)
- [Ejemplo de implementación de archivos temporales en una aplicación web](https://medium.com/@colinrubbert/php-tmp-file-security-e5fd4aecc2be/) (en inglés)