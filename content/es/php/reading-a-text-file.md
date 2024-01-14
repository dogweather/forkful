---
title:    "PHP: Leyendo un archivo de texto"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué leer un archivo de texto?

Leer un archivo de texto es una habilidad esencial en la programación, ya que nos permite acceder y manipular datos almacenados en formato texto. Además, es una de las operaciones más comunes en el desarrollo de aplicaciones web y de escritorio.

## Cómo leer un archivo de texto

Para empezar a leer un archivo de texto en PHP, utilizamos la función `file_get_contents()`. Esta función toma como argumento la ruta del archivo y devuelve una cadena de texto con el contenido del archivo. Por ejemplo:

```
<?php

$texto = file_get_contents('archivo.txt');

echo $texto;
```

Este código leerá el contenido del archivo llamado "archivo.txt" y lo imprimirá en la pantalla.

Si queremos leer el archivo línea por línea, podemos utilizar la función `file()` en su lugar. Esta función devolverá un arreglo donde cada elemento es una línea del archivo. Por ejemplo:

```
<?php

$lineas = file('archivo.txt');

foreach ($lineas as $linea) {
    echo $linea;
}
```

Este código imprimirá cada línea del archivo en una nueva línea.

## Profundizando en la lectura de archivos de texto

Además de las funciones mencionadas anteriormente, existen otras formas de leer archivos de texto en PHP, como utilizar la clase `SplFileObject` o la función `fopen()`. Es importante tener en cuenta que en cualquier caso, es necesario cerrar el archivo después de haberlo leído, utilizando la función `fclose()`.

También hay que tener en cuenta algunos aspectos como la codificación del archivo y el manejo de errores al leer el archivo.

## Ver también

- [Documentación oficial de PHP sobre lectura de archivos](https://www.php.net/manual/es/function.file-get-contents.php)
- [Tutorial de código facilito sobre lectura de archivos de texto en PHP](https://codigofacilito.com/articulos/lectura-escritura-archivos-php)
- [Ejemplo práctico de lectura de archivos de texto en PHP](https://www.anerbarrena.com/leer-archivo-con-php-2014/)