---
title:                "Leyendo un archivo de texto"
html_title:           "PHP: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cómo interactúan las páginas web con los archivos de datos? Bueno, en este artículo te explicaremos cómo leer un archivo de texto en PHP.

## Cómo hacerlo

Para leer un archivo de texto en PHP, primero debes abrir el archivo con la función `fopen()`. Luego, utiliza la función `fgets()` para leer cada línea del archivo y almacenarla en una variable. Puedes utilizar un bucle `while` para seguir leyendo líneas hasta que se alcance el final del archivo. A continuación, puedes imprimir los datos utilizando la función `echo`.

```PHP
<?php
$archivo = fopen("mi_archivo.txt", "r"); // abre el archivo
while(!feof($archivo)) { // itera hasta que se alcance el final del archivo
  $linea = fgets($archivo); // lee una línea del archivo
  echo $linea; // imprime la línea
}
fclose($archivo); // cierra el archivo
?>
```

La salida para un archivo de texto con el siguiente contenido:

```
Hola
soy un archivo de texto
en PHP.
```

sería:

```
Hola
soy un archivo de texto
en PHP.
```

## Profundizando

La función `fopen()` puede recibir un segundo parámetro, que especifica el modo en que se abrirá el archivo. Por ejemplo, si utilizamos "w" en lugar de "r", se abrirá el archivo en modo escritura, lo que nos permite escribir en él.

Además, la función `fopen()` también puede recibir una URL en lugar de un nombre de archivo, permitiendo leer archivos de texto de sitios web.

Para obtener más información sobre cómo leer y escribir archivos de texto en PHP, puedes consultar la documentación oficial de PHP.

## Ver también

- Documentación oficial de PHP sobre manipulación de archivos de texto en PHP: https://www.php.net/manual/es/function.fopen.php