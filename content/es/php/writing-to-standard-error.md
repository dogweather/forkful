---
title:                "PHP: Redactando en el error estándar"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Por qué escribir a la salida de error en PHP

Escribir a la salida de error (stderr) en PHP es una técnica importante que puede ayudar a los programadores a depurar y solucionar problemas en sus códigos. Al identificar y registrar errores, los desarrolladores pueden encontrar rápidamente y corregir los errores en sus aplicaciones PHP.

## Cómo hacerlo

Para escribir a la salida de error en PHP, se puede utilizar la función ```error_log()```. Esta función acepta dos parámetros, el mensaje de error y una constante que indica la ubicación donde se debe escribir el error. En este caso, usaremos la constante ```E_STRERR```, que indica que el error debe ser enviado a la salida de error.

```PHP
<?php
$message = "¡Este es un mensaje de error!";
error_log($message, E_STRERR);
```

Al ejecutar este código, el mensaje de error se enviará a la salida de error, que normalmente se muestra en la consola o en un archivo de registro.

## Profundizando

Además de utilizar la función ```error_log()```, también se pueden escribir errores personalizados en la salida de error utilizando la función ```fwrite()```. Esta función acepta tres parámetros, el descriptor de archivo, el mensaje de error y el tamaño del mensaje. Usando el descriptor de archivo ```STDERR```, podemos escribir mensajes de error directamente en la salida de error.

```PHP
<?php
$message = "¡Este es un mensaje de error personalizado!";
fwrite(STDERR, $message);
```

También es importante mencionar que es posible escribir a la salida de error desde cualquier lugar en el código, no solo en las secciones de manejo de errores. Esto puede ser útil para registrar errores y realizar un seguimiento de la ejecución de su código.

# Ver también

- Documentación oficial de PHP sobre la función ```error_log()```: https://www.php.net/manual/en/function.error-log.php
- Documentación oficial de PHP sobre la constante ```E_STRTERR```: https://www.php.net/manual/en/errorfunc.constants.php
- Tutorial de Tuts+: https://code.tutsplus.com/tutorials/writing-logging-to-the-php-standard-error-outputs--cms-28261