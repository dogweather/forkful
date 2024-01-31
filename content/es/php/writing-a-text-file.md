---
title:                "Escritura de un archivo de texto"
date:                  2024-01-19
html_title:           "Bash: Escritura de un archivo de texto"
simple_title:         "Escritura de un archivo de texto"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir un archivo de texto en PHP significa almacenar información en un archivo en el disco, algo esencial para guardar datos, configuraciones o logs. Los programadores lo hacen para preservar información entre sesiones, compartir datos con otros programas o generar reportes legibles por humanos.

## Cómo hacerlo:
Escribir en un archivo es simple. Usa `file_put_contents()` para escribir texto rápidamente, o `fopen()` seguido de `fwrite()` y `fclose()` para más control.

```PHP
<?php
$texto = "Hola, este es un archivo de texto.";

// Opción simple: file_put_contents()
file_put_contents("archivo.txt", $texto);

// Opción avanzada: fopen(), fwrite(), fclose()
$archivo = fopen("archivo_detallado.txt", "w") or die("No se pudo abrir el archivo!");
fwrite($archivo, $texto);
fclose($archivo);
?>
```

Si ejecutas este código, se crearán dos archivos: "archivo.txt" y "archivo_detallado.txt", ambos con el contenido "Hola, este es un archivo de texto.".

## Inmersión Profunda:
En PHP, la escritura de archivos ha evolucionado desde `fopen()` y sus funciones asociadas, siendo la forma tradicional, hasta funciones modernas como `file_put_contents()` que simplifica el proceso. Alternativamente, las clases como `SplFileObject` ofrecen una orientación a objetos para trabajar con archivos. Mientras que `file_put_contents()` es adecuado para escrituras directas y cortas, `fopen()` y compañía son mejores para manipulaciones complejas y controladas de archivos.

## Ver También:
- [Documentación oficial de PHP sobre el manejo de ficheros](https://www.php.net/manual/es/book.filesystem.php)
- [file_put_contents() - Manual PHP](https://www.php.net/manual/es/function.file-put-contents.php)
- [fopen() - Manual PHP](https://www.php.net/manual/es/function.fopen.php)
- [Clase SplFileObject - Manual PHP](https://www.php.net/manual/es/class.splfileobject.php)
