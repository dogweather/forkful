---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué? 
Leer un archivo de texto en programación significa extraer información de un archivo plano. Programadores lo hacen porque es una forma eficiente y sencilla de procesar y analizar datos almacenados en archivos rústicos para propósitos diversos. 

## ¿Cómo hacerlo? 
Acá te muestro cómo puedes leer un archivo de texto en PHP:

```PHP
<?php
$archivo = 'ruta/archivo.txt';

// Leer todo el archivo
$contenido = file_get_contents($archivo);
echo $contenido;

// Leer el archivo línea por línea
$lineas = file($archivo);
foreach ($lineas as $linea_num => $linea) {
    echo 'Línea #<b>'.$linea_num.'</b> : ' . htmlspecialchars($linea) . "<br>\n";
}
?>
```
Esto imprimirá el contenido del archivo 'archivo.txt', línea por línea.

## Profundizando 
- **Contexto histórico**: PHP se desarrolló en 1995, inventado por Rasmus Lerdorf. Desde sus inicios, PHP ha sido útil para gestionar archivos de texto. Su habilidad para leer y escribir archivos ha facilitado su uso en una amplia gama de aplicaciones, desde blogs y sitios web hasta sistemas de gestión de contenido y bases de datos.

- **Alternativas**: Aunque `file_get_contents()` y `file()` son funciones utilitarias para leer archivos de texto en PHP, hay otras opciones. La función `fopen()` te permite abrir un archivo de texto y usar `fread()` para leerlo. Si necesitas más control sobre el flujo de datos, esta podría ser tu opción.

```PHP
<?php
$archivo = fopen("ruta/archivo.txt", "r");
if ($archivo) {
    while (($linea = fgets($archivo)) !== false) {
        echo $linea;
    }
    fclose($archivo);
} else {
    echo 'Error al abrir el archivo.';
} 
?>
```
- **Detalles de implementación**: La función `file_get_contents()` lee todo el archivo en memoria a la vez, lo que puede ser un problema para archivos grandes. En cambio, `file()` divide el archivo en una matriz de líneas, mientras que el uso de `fopen()` y `fgets()` te permite controlar cuánto del archivo leer a la vez.

## Ver También 
Asegúrate de revisar la [documentación oficial de PHP en español](https://www.php.net/manual/es/) sobre la gestión de archivos para una comprensión más profunda. También, puedes visitar [PHP: El camino correcto](https://phptherightway.com/#pdo_extension), que ofrece guías actualizadas en español de las mejores prácticas y estándares recomendados en PHP.