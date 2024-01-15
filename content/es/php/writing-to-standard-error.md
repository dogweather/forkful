---
title:                "Escribiendo en el error estándar"
html_title:           "PHP: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir a la salida de error estándar?

Escribir a la salida de error estándar es una forma eficaz de depuración y manejo de errores en la programación PHP. Al hacerlo, los errores se envían a un archivo de registro en lugar de ser impresos en la pantalla, lo que hace que sea más fácil de rastrear y solucionar problemas en el código.

## Cómo hacerlo

Para escribir a la salida de error estándar en PHP, se utiliza la función `fwrite()`. A continuación se muestra un ejemplo de cómo utilizarla en un código:

```
<?php
$file = "errores.log"; // Nombre del archivo de registro
$error = "Error: no se puede dividir por cero"; // Mensaje de error

$handle = fopen($file, 'a+'); // Abre el archivo en modo de escritura
fwrite($handle, $error); // Escribe el error en el archivo
fclose($handle); // Cierra el archivo

echo "Se ha producido un error. Consulte el archivo de registro para obtener más detalles."; // Mensaje para el usuario
?>
```

El código anterior escribirá el mensaje de error en el archivo "errores.log". Si el archivo no existe, se creará automáticamente. También es posible agregar la fecha y hora del error utilizando la función `date()` para obtener un registro más detallado.

Puedes utilizar la función `error_reporting()` para especificar qué tipos de errores quieres que se escriban en el archivo de registro. Por defecto, todos los errores estarán incluidos, pero puedes personalizarlo según tus necesidades.

## Profundizando en la escritura a la salida de error estándar

Existen diferentes razones por las que querrías escribir a la salida de error estándar en lugar de simplemente imprimir los errores en la pantalla. Una de las principales razones es cuando tienes un sitio web en producción y no quieres que los usuarios vean los errores en la página, ya que puede causar confusión o alertar a los posibles atacantes sobre posibles vulnerabilidades en tu código.

Otra razón para escribir a la salida de error estándar es que los errores y avisos pueden pasar desapercibidos en la página web, especialmente si hay una gran cantidad de código en ejecución. Al escribirlos en un archivo de registro, puedes revisarlo más tarde y solucionar cualquier problema que se haya producido.

Además, la escritura a la salida de error estándar es especialmente útil en situaciones en las que no tienes acceso al servidor o a los registros del servidor, como en un hosting compartido.

## Véase también

- [Función fwrite() en PHP](https://www.php.net/manual/es/function.fwrite.php)
- [Función error_reporting() en PHP](https://www.php.net/manual/es/function.error-reporting.php)
- [Documentación de PHP sobre el manejo de errores](https://www.php.net/manual/es/ref.errorfunc.php)