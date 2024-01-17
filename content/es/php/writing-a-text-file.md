---
title:                "Redactando un archivo de texto"
html_title:           "PHP: Redactando un archivo de texto"
simple_title:         "Redactando un archivo de texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Escribir un archivo de texto en PHP es una tarea común para los programadores. Se trata simplemente de crear un archivo en formato de texto plano, que puede ser leído y editado fácilmente. Los programadores lo hacen para almacenar y organizar información de manera estructurada, como datos de configuración o registros de actividad.

## Cómo hacerlo:
```
<?php
// Abrir un archivo en modo de escritura ("w" significa escribir en inglés)
$file = fopen("ejemplo.txt", "w") or die("No se puede abrir archivo!");
// Escribir texto en el archivo
$txt = "Hola Mundo!";
fwrite($file, $txt);
// Cerrar el archivo
fclose($file);
// Leer el archivo
echo file_get_contents("ejemplo.txt");
?>
```
Output:
```
Hola Mundo!
```

## Profundizando:
Escribir archivos de texto en programas de computadora ha sido una práctica común desde los inicios de la informática. Los primeros sistemas de almacenamiento de información utilizaban tarjetas perforadas para escribir y guardar datos. En la actualidad, existen varias alternativas para escribir archivos de texto en PHP, como la función `file_put_contents()` o la clase `SplFileObject`. Además, es importante tener en cuenta la codificación de los caracteres al leer y escribir archivos de texto, especialmente si se trabaja con diferentes idiomas.

## Ver más:
- [Documentación de PHP sobre la función `fwrite()`](https://www.php.net/manual/es/function.fwrite.php)
- [Tutorial de W3Schools sobre escribir archivos en PHP](https://www.w3schools.com/php/php_file_create.asp)
- [Guía de PHP.net sobre cómo manejar archivos de texto](https://www.php.net/manual/es/ref.filesystem.php)