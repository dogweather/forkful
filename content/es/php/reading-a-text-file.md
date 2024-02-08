---
title:                "Lectura de un archivo de texto"
aliases:
- es/php/reading-a-text-file.md
date:                  2024-01-20T17:54:42.931208-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lectura de un archivo de texto"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Leer un archivo de texto en PHP implica obtener su contenido para manipulación o análisis. Los programadores hacen esto para importar datos, configuraciones o simplemente mostrar texto en la web.

## How to:
```PHP
<?php
$archivo = 'miTexto.txt'; // Reemplaza con tu propio nombre de archivo

// Verifica si el archivo existe y es legible
if (is_readable($archivo)) {
    // Opción 1: Leer todo el texto de una vez
    $contenido = file_get_contents($archivo);
    echo $contenido;

    // Opción 2: Leer línea por línea
    $fp = fopen($archivo, 'r');
    if ($fp) {
        while (($linea = fgets($fp)) !== false) {
            echo $linea;
        }
        if (!feof($fp)) {
            echo "Error: no se pudo leer el archivo hasta el final\n";
        }
        fclose($fp);
    }
} else {
    echo "El archivo no existe o no tiene permisos de lectura.";
}
?>
```
Output:
```
Contenido de miTexto.txt...
```

## Deep Dive:
Historically, PHP has evolved its file-reading functions, increasing efficiency and ease of use. `file_get_contents` es perfecto para archivos pequeños, mientras que `fopen` y `fgets` son mejores para archivos grandes, ya que leen línea por línea.

Alternativas incluyen `file()`, que lee un archivo completo y lo convierte en un array, y `SplFileObject` para una orientación más orientada a objetos.

En la implementación, considera el manejo de errores con funciones como `is_readable` y verifica el final del archivo con `feof` para prevenir la lectura de archivos incompletos.

## See Also:
- [PHP Official Documentation on file_get_contents](https://www.php.net/manual/en/function.file-get-contents.php)
- [PHP Official Documentation on fopen](https://www.php.net/manual/en/function.fopen.php)
- [PHP.net - Handling file uploads](https://www.php.net/manual/en/features.file-upload.php)
- [PHP The Right Way - Working With Files](https://phptherightway.com/#files)
