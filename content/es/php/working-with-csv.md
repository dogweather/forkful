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

## ¿Por qué trabajar con CSV?

Trabajar con archivos CSV (Comma Separated Values) es una tarea común en el desarrollo web. Es una forma eficiente y sencilla de almacenar datos en forma de tabla y compartirlos entre diferentes aplicaciones. Con PHP, podemos manipular y procesar estos archivos de manera efectiva, lo que facilita la gestión y uso de grandes cantidades de datos.

## Cómo hacerlo

Una de las formas más sencillas de trabajar con CSV en PHP es mediante el uso de las funciones `fopen()`, `fputcsv()` y `fclose()`. Estas funciones nos permiten abrir un archivo CSV, escribir datos en él y cerrarlo una vez que hayamos terminado.

Primero, abrimos un archivo CSV existente o creamos uno nuevo utilizando `fopen()`, proporcionando el nombre del archivo y el modo de apertura. Por ejemplo:

```PHP
$archivo = fopen("datos.csv", "w");
```

El modo "w" indica que el archivo se abrirá en modo de escritura, lo que significa que podemos escribir datos en él. Ahora podemos utilizar `fputcsv()` para escribir una fila de datos en el archivo. Por ejemplo:

```PHP
fputcsv($archivo, ["Nombre", "Apellido", "Edad"]);
```

Esto escribirá una fila en el archivo CSV con los valores "Nombre", "Apellido" y "Edad" separados por comas.

Para escribir múltiples filas de datos, podemos utilizar un bucle y pasar un array con los valores de cada fila a `fputcsv()`. Luego, cerramos el archivo utilizando `fclose()`. Por ejemplo:

```PHP
$personas = [
    ["Juan", "Pérez", "25"],
    ["María", "González", "30"],
    ["Carlos", "López", "40"]
];

foreach ($personas as $persona) {
    fputcsv($archivo, $persona);
}

fclose($archivo);
```

Esto escribirá tres filas en el archivo CSV con los datos de las tres personas.

Otra opción para trabajar con CSV en PHP es utilizando la función `str_getcsv()`, que nos permite leer una fila del archivo CSV y convertirla en un array de valores. Por ejemplo:

```PHP
$archivo = fopen("datos.csv", "r");

while (($fila = fgetcsv($archivo)) !== false) {
    // Hacer algo con los datos de la fila
}

fclose($archivo);
```

Como se puede ver, esta función es particularmente útil para leer grandes cantidades de datos de un archivo CSV y realizar operaciones en ellos.

## Profundizando

Trabajar con CSV también nos brinda la posibilidad de utilizar la función `fgetcsv()` junto con la función `filter_var()` para validar los datos de nuestro archivo. Por ejemplo, podemos validar que un campo contenga un número entero antes de guardarlo en nuestra base de datos. Esto nos ayuda a garantizar la integridad de nuestros datos y evitar posibles errores.

Otra técnica útil es utilizar el delimitador de campos personalizado en lugar de una coma. Por ejemplo, si nuestro archivo CSV utiliza tabulaciones en lugar de comas para separar los valores, podemos especificar esto utilizando el parámetro `delimitor` en `fgetcsv()`. Esto nos permite procesar diferentes tipos de archivos CSV con facilidad.

## Ver también

- [Documentación oficial de PHP sobre CSV](https://www.php.net/manual/es/function.fputcsv.php)
- [Cómo trabajar con archivos CSV en PHP](https://www.w3schools.com/php/php_file_open.asp)
- [Ejemplo de validación de datos CSV en PHP](https://www.phpzag.com/read-and-parse-csv-file-using-php-example/)