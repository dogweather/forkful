---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:31.263756-07:00
description: "C\xF3mo hacerlo: PHP ofrece funciones incorporadas para manejar archivos\
  \ CSV, lo que facilita leer y escribir en estos archivos sin necesidad de bibliotecas\u2026"
lastmod: '2024-03-13T22:44:59.182875-06:00'
model: gpt-4-0125-preview
summary: PHP ofrece funciones incorporadas para manejar archivos CSV, lo que facilita
  leer y escribir en estos archivos sin necesidad de bibliotecas de terceros.
title: Trabajando con CSV
weight: 37
---

## Cómo hacerlo:
PHP ofrece funciones incorporadas para manejar archivos CSV, lo que facilita leer y escribir en estos archivos sin necesidad de bibliotecas de terceros. Aquí hay ejemplos para comenzar:

### Leyendo un Archivo CSV
Puedes abrir un archivo CSV y leer su contenido usando `fopen()` en combinación con `fgetcsv()`:

```php
<?php
$filename = 'data.csv';
$handle = fopen($filename, "r");
if ($handle !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        $num = count($data);
        echo "Número de campos en la línea: $num\n";
        for ($c = 0; $c < $num; $c++) {
            echo $data[$c] . "\n";
        }
    }
    fclose($handle);
}
?>
```

Este script imprime el número de campos de cada línea seguido del contenido de cada campo.

### Escribiendo en un Archivo CSV
Para escribir en un archivo CSV, usa `fopen()` en modo de escritura (`w`) y `fputcsv()`:

```php
<?php
$list = [
    ['ID', 'Nombre', 'Correo'],
    [1, 'John Doe', 'john@example.com'],
    [2, 'Jane Doe', 'jane@example.com']
];

$handle = fopen('users.csv', 'w');

foreach ($list as $row) {
    fputcsv($handle, $row);
}

fclose($handle);
?>
```

Este script crea un archivo llamado `users.csv` y escribe el encabezado y dos filas de datos en él.

### Usando una Biblioteca: League\Csv
Para un manejo más avanzado de CSV, la biblioteca `League\Csv` ofrece un robusto conjunto de características. Después de instalarla a través de Composer (`composer require league/csv`), puedes usarla para leer y escribir datos CSV de manera más flexible.

#### Leyendo con League\Csv
```php
<?php
require 'vendor/autoload.php';

use League\Csv\Reader;

$csv = Reader::createFromPath('data.csv', 'r');
$csv->setHeaderOffset(0); // Establece si quieres usar la primera fila como encabezado

$results = $csv->getRecords();
foreach ($results as $row) {
    print_r($row);
}
?>
```

Este script lee `data.csv`, tratando la primera fila como encabezados de columnas e imprime cada fila como un arreglo asociativo.

#### Escribiendo con League\Csv
```php
<?php
require 'vendor/autoload.php';

use League\Csv\Writer;

$csv = Writer::createFromPath('users_new.csv', 'w+');

$csv->insertOne(['ID', 'Nombre', 'Correo']);
$csv->insertAll([
    [3, 'Alex Doe', 'alex@example.com'],
    [4, 'Anna Smith', 'anna@example.com']
]);

echo "Escrito en users_new.csv exitosamente.";
?>
```

Esto crea `users_new.csv` y escribe una fila de encabezado seguida por dos filas de datos.
