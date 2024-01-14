---
title:                "PHP: Trabajando con archivos csv"
simple_title:         "Trabajando con archivos csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/working-with-csv.md"
---

{{< edit_this_page >}}

##Por qué

Trabajar con archivos CSV es una habilidad importante para cualquier programador de PHP. Los archivos CSV son comúnmente utilizados para almacenar grandes cantidades de datos en un formato fácil de manipular. Al dominar el trabajo con CSV, podrás mejorar tu eficiencia en la programación y manejar grandes conjuntos de datos con mayor facilidad.

##Cómo hacerlo

Para trabajar con archivos CSV en PHP, utilizaremos la función `fgetcsv()` que lee una línea del archivo y la devuelve como un array. También utilizaremos la función `fopen()` para abrir el archivo en modo de lectura. Aquí hay un ejemplo de código para leer un archivo CSV y mostrar su contenido en una tabla HTML:

```
<?php

//abrir el archivo CSV
$archivo = fopen("datos.csv", "r");

//leer la primera línea del archivo como los encabezados de la tabla
$headers = fgetcsv($archivo);

//imprimir la tabla HTML
echo "<table>";

//imprimir los encabezados de la tabla
echo "<tr>";
foreach ($headers as $header) {
    echo "<th>" . $header . "</th>";
}
echo "</tr>";

//iterar a través de las líneas del archivo y mostrar los datos en filas
while (($datos = fgetcsv($archivo)) !== false) {
    echo "<tr>";
    foreach ($datos as $dato) {
        echo "<td>" . $dato . "</td>";
    }
    echo "</tr>";
}

echo "</table>";

//cerrar el archivo
fclose($archivo);

?>
```

Este código abrirá el archivo "datos.csv" y lo mostrará en una tabla HTML. El primer valor de cada línea se convertirá en el encabezado de la tabla, mientras que los demás valores serán los datos de cada fila. 

Si queremos añadir una nueva línea de datos al archivo CSV, podemos utilizar la función `fputcsv()` que añade una nueva línea al final del archivo. Aquí hay un ejemplo de código para agregar una nueva línea al archivo CSV:

```
<?php

//abrir el archivo CSV en modo lectura y escritura
$archivo = fopen("datos.csv", "a+");

//array con los datos para agregar al archivo
$nueva_fila = array("Juan", "Gómez", "27");

//añadir la nueva línea al archivo CSV
fputcsv($archivo, $nueva_fila);

//cerrar el archivo
fclose($archivo);

?>
```

##Profundizando

Una vez que tengas dominado el trabajo básico con archivos CSV en PHP, puedes explorar funciones más avanzadas como `fseek()` para desplazarte a una línea específica del archivo o `fgetcsv()` para leer solo ciertas columnas en lugar de toda la línea. También puedes utilizar la función `array_chunk()` para dividir el archivo CSV en arrays más pequeños.

Es importante tener en cuenta que los archivos CSV pueden tener diferentes formatos, como por ejemplo el delimitador de valores puede ser una coma o un punto y coma. Por ello, es recomendable utilizar la función `ini_set()` para configurar el delimitador correcto al leer un archivo CSV. 

##Ver también

- [Documentación oficial de PHP sobre trabajo con archivos CSV](https://www.php.net/manual/es/function.fgetcsv.php)
- [Tutorial de W3Schools sobre archivos CSV en PHP](https://www.w3schools.com/php/php_file_csv.asp)
- [Artículo de Medium sobre cómo trabajar con archivos CSV en PHP](https://medium.com/@llatin26/read-and-write-csv-file-in-php-f8f12c2e0138)