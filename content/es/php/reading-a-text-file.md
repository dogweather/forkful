---
title:    "PHP: Leyendo un archivo de texto"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## ¿Por qué deberías aprender a leer archivos de texto en PHP?

Si eres un programador PHP, es importante que sepas cómo leer archivos de texto para poder manipular y procesar datos en tus aplicaciones. Esto te permitirá crear scripts más dinámicos y eficientes que puedan leer y escribir información en archivos de texto.

## Cómo leer archivos de texto en PHP

Leer un archivo de texto en PHP es bastante sencillo. Solo necesitas seguir unos pocos pasos:

```PHP
// Abrir el archivo en modo lectura
$file = fopen("miarchivo.txt", "r");

// Leer el contenido del archivo
// y guardarlo en una variable
$file_content = fread($file, filesize("miarchivo.txt"));

// Imprimir el contenido del archivo
echo $file_content;

// Cerrar el archivo
fclose($file);
```

El código anterior abre el archivo en modo lectura, lo guarda en una variable y lo imprime en pantalla. También es importante cerrar el archivo después de leerlo para liberar los recursos.

## Profundizando en la lectura de archivos de texto en PHP

Se pueden realizar muchas operaciones interesantes con la lectura de archivos de texto en PHP. Por ejemplo, puedes leer solo una línea a la vez utilizando la función `fgets()`, o buscar una palabra específica dentro del archivo usando `strpos()`. También puedes leer archivos de texto en formato CSV y separar los datos en columnas utilizando `explode()`.

También es importante entender que cuando lees un archivo de texto en PHP, el contenido se guarda en memoria y puede ser manipulado fácilmente. Por lo tanto, si deseas realizar cambios en el archivo, puedes escribir directamente en él utilizando la función `fwrite()`.

## Ver también

- [PHP: fopen()](https://www.php.net/manual/es/function.fopen.php)
- [PHP: fread()](https://www.php.net/manual/es/function.fread.php)
- [PHP: fclose()](https://www.php.net/manual/es/function.fclose.php)