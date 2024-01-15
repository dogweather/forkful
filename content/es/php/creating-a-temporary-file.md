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

## ¿Por qué?

Crear un archivo temporal en PHP puede ser útil cuando necesitas almacenar datos temporalmente y no quieres comprometer un archivo permanente en el servidor. Este método también es comúnmente utilizado en pruebas y depuración de código.

## ¿Cómo hacerlo?

Para crear un archivo temporal en PHP, puedes utilizar la función `tempnam()` que acepta dos parámetros: el directorio donde quieres que se cree el archivo y un prefijo opcional para el nombre del archivo.

```PHP
$archivo_temporal = tempnam('/temp', 'temp_file_');
echo $archivo_temporal;
```

El código anterior creará un archivo llamado `temp_file_xxxxxxxxx` en la carpeta `/temp`. El `xxxxxxxxx` corresponderá a un identificador único generado automáticamente por PHP.

Si no especificas un directorio, se utilizará el directorio predeterminado para almacenar archivos temporales del servidor. También puedes utilizar la función `sys_get_temp_dir()` para obtener el directorio predeterminado en tu servidor.

Para escribir datos en el archivo temporal, puedes utilizar la función `fwrite()`:

```PHP
$archivo_temporal = tempnam(sys_get_temp_dir(), 'temp_file_');
$archivo = fopen($archivo_temporal, 'w');
fwrite($archivo, 'Datos de ejemplo');
fclose($archivo);
```

Para leer los datos del archivo temporal, puedes utilizar la función `file_get_contents()`:

```PHP
$datos = file_get_contents($archivo_temporal);
var_dump($datos);
```

## Profundizando

Es importante tener en cuenta que un archivo temporal creado con `tempnam()` se eliminará automáticamente una vez que el script termine de ejecutarse. Si necesitas que el archivo permanezca para su uso posterior, puedes utilizar la función `tmpfile()` en su lugar.

Además, ten en cuenta que la función `tempnam()` sólo crea el archivo y no lo abre automáticamente. Debes utilizar la función `fopen()` para abrir el archivo y escribir en él.

## Ver también

- Documentación oficial de PHP sobre la función `tempnam()`: https://www.php.net/manual/es/function.tempnam.php
- Tutorial sobre cómo crear y trabajar con archivos temporales en PHP: https://www.w3schools.com/php/php_tempnam.asp