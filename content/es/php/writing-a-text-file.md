---
title:                "Escribiendo un archivo de texto"
html_title:           "PHP: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Escribir un archivo de texto puede ser una tarea útil y necesaria para cualquier programador en PHP. Ya sea para almacenar datos relevantes o generar informes, esta habilidad te permite guardar información de manera persistente y acceder a ella en cualquier momento.

## Cómo hacerlo

Para escribir un archivo de texto en PHP, primero debes abrirlo utilizando la función `fopen()` y especificando el nombre del archivo y su modo (lectura, escritura, etc.). Luego, utiliza la función `fwrite()` para escribir en el archivo y `fclose()` para cerrarlo y guardar los cambios.

Veamos un ejemplo de cómo escribir en un archivo llamado "datos.txt":

```PHP
// Abrir el archivo en modo escritura
$archivo = fopen("datos.txt", "w");

// Escribir una línea de texto en el archivo
fwrite($archivo, "¡Hola Mundo!");

// Cerrar el archivo y guardar los cambios
fclose($archivo);
```

Si deseas agregar más líneas a tu archivo existente, puedes abrirlo en modo "añadir" en lugar de "escritura". También puedes escribir variables o valores separados por comas utilizando la función `implode()`.

## Profundizando

Escribir un archivo de texto puede ser más complejo en ciertas situaciones, como cuando se manejan grandes cantidades de datos o se necesita un formato específico. En estos casos, es posible utilizar funciones como `fputcsv()` para escribir datos en formato CSV o `file_put_contents()` para escribir una cadena completa en un archivo.

Otra funcionalidad útil es el uso de la función `chmod()` para cambiar los permisos de un archivo y controlar quién puede leer o escribir en él.

## Ver también

- [Documentación oficial de PHP sobre escribir archivos de texto](https://www.php.net/manual/es/function.fwrite.php)
- [Tutorial de Codecademy sobre escribir archivos en PHP](https://www.codecademy.com/es/forum_questions/55c15b40d3292f193d001182)