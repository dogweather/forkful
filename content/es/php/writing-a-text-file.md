---
title:                "PHP: Escribiendo un archivo de texto"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué escribir un archivo de texto

Muchas veces en la programación es necesario almacenar información en un archivo, ya sea para guardarlo para futuras referencias o para compartirlo con otros usuarios. Escribir un archivo de texto es una forma sencilla y eficiente de lograr esto.

## Cómo hacerlo

Para escribir un archivo de texto en PHP, primero debemos abrirlo utilizando la función `fopen()` y especificando el modo de apertura como "w", que significa "write" o escribir. Luego, podemos utilizar la función `fwrite()` para escribir en el archivo. A continuación, cerramos el archivo con `fclose()`.

```PHP
$archivo = fopen("mi_archivo.txt", "w") or die("No se pudo abrir el archivo"); //Abrir archivo
$txt = "Este es un ejemplo de texto que se escribirá en el archivo."; //Texto a escribir
fwrite($archivo, $txt); //Escribir texto en el archivo
fclose($archivo); //Cerrar archivo
```

El código anterior creará un archivo llamado "mi_archivo.txt" y escribirá el texto indicado en él. Si deseamos agregar más texto al archivo en lugar de reemplazarlo, podemos utilizar el modo de apertura "a" en lugar de "w".

## Profundizando

Además de escribir texto, también podemos escribir variables en un archivo de texto. Por ejemplo:

```PHP
$nombre = "Juan";
$edad = 25;
$archivo = fopen("mi_archivo.txt", "w") or die("No se pudo abrir el archivo"); //Abrir archivo
$txt = "Nombre: " . $nombre . "\nEdad: " . $edad; //Texto a escribir
fwrite($archivo, $txt); //Escribir texto en el archivo
fclose($archivo); //Cerrar archivo
```

Este código creará un archivo con el nombre y la edad especificados en variables. También podemos utilizar la función `file_put_contents()` para escribir en un archivo sin tener que abrir y cerrar manualmente el archivo.

Ahora que sabemos cómo escribir en un archivo de texto, es importante recordar que debemos asegurarnos de tener los permisos adecuados para escribir en el archivo. Podemos cambiar los permisos utilizando herramientas como FileZilla o a través de la línea de comandos utilizando el comando `chmod`.

## Ver también

- [fopen() en PHP](https://www.php.net/manual/es/function.fopen.php)
- [fwrite() en PHP](https://www.php.net/manual/es/function.fwrite.php)
- [fclose() en PHP](https://www.php.net/manual/es/function.fclose.php)
- [file_put_contents() en PHP](https://www.php.net/manual/es/function.file-put-contents.php)
- [Comando chmod](https://www.linux.com/learn/chmod-versatile-command-setting-permissions-linux)