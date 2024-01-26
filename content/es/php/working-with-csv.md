---
title:                "Trabajando con archivos CSV"
html_title:           "Bash: Trabajando con archivos CSV"
simple_title:         "Trabajando con archivos CSV"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Trabajar con CSV implica manejar datos en un formato de texto plano delimitado por comas, muy común por su simplicidad y universalidad. Los programadores lo utilizan para intercambiar datos entre sistemas, importar/exportar información y manipular grandes volúmenes de datos de manera eficiente.

## Cómo Hacerlo:

Leer un archivo CSV:
```PHP
<?php
$fila = 1;
if (($handle = fopen("datos.csv", "r")) !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        $num = count($data);
        echo "Fila $fila: \n";
        for ($c=0; $c < $num; $c++) {
            echo $data[$c] . "\n";
        }
        $fila++;
    }
    fclose($handle);
}
?>
```

Escribir en un archivo CSV:
```PHP
<?php
$list = array (
    array('primero', 'segundo', 'tercero'),
    array('uno', 'dos', 'tres'),
);

$fp = fopen('file.csv', 'w');

foreach ($list as $fields) {
    fputcsv($fp, $fields);
}

fclose($fp);
?>
```

## Profundizando

El formato CSV se originó en los primeros días de las computadoras personales. Más que un estándar rígido, CSV es un convenio práctico y su simplicidad ha permitido que sea ampliamente adoptado. Aunque existen alternativas como XML y JSON, CSV se sigue usando por ser menos verboso y más fácil de leer por humanos y máquinas. Al implementarlo en PHP, es importante considerar el manejo de caracteres especiales, como comillas o saltos de línea dentro de los campos, y la configuración regional que afecta cómo se interpretan las comas y puntos.

## Ver También

- Documentación oficial de PHP para la función `fgetcsv`: https://www.php.net/manual/es/function.fgetcsv.php
- Documentación oficial de PHP para la función `fputcsv`: https://www.php.net/manual/es/function.fputcsv.php
- Tutorial de W3Schools sobre cómo manejar archivos CSV en PHP: https://www.w3schools.com/php/php_file_create.asp
