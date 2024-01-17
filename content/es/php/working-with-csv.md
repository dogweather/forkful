---
title:                "Trabajando con csv"
html_title:           "PHP: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Trabajar con CSV en PHP es una forma de manejar datos tabulares de forma eficiente y sencilla. Programadores utilizan la extensión "csv" en PHP para leer, escribir y manipular archivos CSV, lo que permite el procesamiento de datos en formato tabla de manera más efectiva.

## Cómo hacerlo:

Para leer un archivo CSV en PHP, se utiliza la función `fgetcsv()` que toma como argumento el nombre del archivo y devuelve un array. Por ejemplo:

```PHP
$archivo = fopen("datos.csv", "r");
while(! feof($archivo)) {
  print_r(fgetcsv($archivo));
}
fclose($archivo);
```

Este código abrirá el archivo "datos.csv", lo leerá línea por línea y convertirá cada línea en un array que será impreso en pantalla. Aquí el resultado que podríamos obtener:

```
Array
(
    [0] => Juan
    [1] => Pérez
    [2] => 25
)
Array
(
    [0] => Ana
    [1] => Gómez
    [2] => 30
)
```

Para escribir un nuevo archivo CSV, se puede utilizar la función `fputcsv()` pasando como argumento el nombre del archivo y un array con los valores que se quieren escribir. Por ejemplo:

```PHP
$archivo = fopen("nuevos_datos.csv", "w");
$array = array("Juan", "Pérez", "25");
fputcsv($archivo, $array);
fclose($archivo);
```

Este código creará un archivo "nuevos_datos.csv" con el siguiente contenido:

```
Juan,Pérez,25
```

## Profundizando

El formato CSV (Comma Separated Values) fue creado en los años 70 para ser utilizado en hojas de cálculo. Hoy en día, sigue siendo ampliamente utilizado en la industria de la programación para el manejo de bases de datos y datos tabulares.

Existen otras opciones para trabajar con datos tabulares en PHP, como por ejemplo utilizar PDO (PHP Data Objects) para conectarse a una base de datos y manejar los datos directamente desde ahí. Sin embargo, utilizar archivos CSV puede ser una opción más sencilla y rápida en ciertos casos.

Para implementar la lectura y escritura de archivos CSV en PHP, también se pueden utilizar funciones como `readfile()`, `fopen()` y `fputcsv()` en conjunto para facilitar el proceso.

## Ver también

- Documentación oficial de PHP sobre la extensión "csv": https://www.php.net/manual/es/book.csv.php
- Tutorial de W3Schools sobre cómo trabajar con archivos CSV en PHP: https://www.w3schools.com/php/func_filesystem_fputcsv.asp