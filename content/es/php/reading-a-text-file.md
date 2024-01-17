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

## ¿Qué y por qué?

La lectura de un archivo de texto es una tarea común en la programación, donde se utiliza para acceder y leer el contenido de un archivo de texto. Los programadores lo hacen para leer y manipular datos almacenados en un archivo de texto, como por ejemplo, configuraciones, registros, y cualquier otro tipo de información.

## ¿Cómo hacerlo?

Aquí hay dos formas diferentes de leer un archivo de texto en PHP. Primero, podemos usar la función `file_get_contents()` para leer el contenido completo del archivo y almacenarlo en una variable. Luego, podemos usar `echo` para imprimir el contenido en la pantalla. Por ejemplo:

```PHP
$file_content = file_get_contents("archivo.txt");
echo $file_content;
```

Otra forma de hacerlo es mediante el uso de la función `fopen()` para abrir el archivo y luego `fgets()` para leer cada línea del archivo. Veamos un ejemplo:

```PHP
$file = fopen("archivo.txt", "r");
while(!feof($file)) {
    $line = fgets($file);
    echo $line;
}
fclose($file);
```

Este último método es útil si necesitamos realizar alguna manipulación en cada línea del archivo antes de imprimirlo en la pantalla.

## Profundizando

La lectura de un archivo de texto es una tarea común en la programación ya que los archivos de texto son una forma fácil y legible de almacenar datos. Esta práctica ha sido utilizada desde los primeros días de la programación de computadoras, y sigue siendo una de las formas más populares de almacenamiento de datos en la actualidad.

Además de `file_get_contents()` y `fopen()`, también podemos usar la función `readfile()` para leer e imprimir un archivo de texto. Sin embargo, esta función no permite la manipulación del contenido del archivo antes de imprimirlo en la pantalla.

Otra alternativa es usar la función `file()` para leer y almacenar cada línea del archivo en un array. De esta manera, podemos luego manipular cada línea individualmente antes de imprimirla en la pantalla.

En cuanto a la implementación, los archivos de texto pueden contener diferentes tipos de caracteres, como espacios en blanco, saltos de línea, etc. Por lo tanto, es importante tener en cuenta estas posibles diferencias al realizar la lectura del archivo.

## Ver también

Si quieres aprender más sobre la manipulación de archivos de texto en PHP, puedes consultar la documentación oficial de PHP sobre funciones de manejo de archivos aquí: https://www.php.net/manual/en/ref.filesystem.php. Además, también puedes visitar la documentación de funciones específicas como `file_get_contents()`, `fopen()`, `fread()`, `fgets()`, entre otras. ¡Explora y diviértete programando en PHP!