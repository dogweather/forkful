---
title:    "PHP: Escribiendo en el error estándar"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

Escribir al error estándar es una práctica común en la programación de PHP que puede ser beneficiosa por varias razones. Por un lado, ayuda a los desarrolladores a identificar y solucionar errores en su código, lo que puede mejorar la calidad del mismo. Además, escribir al error estándar también puede ser útil para depurar y analizar el flujo de ejecución de un programa.

## Cómo hacerlo

Para escribir al error estándar en PHP, se puede utilizar la función `fwrite()` junto con la constante `STDERR`, que representa el descriptor de archivo del error estándar. Un ejemplo de cómo se puede utilizar esta función es el siguiente:

```PHP
<?php
// Abrir el archivo para escribir
$file = fopen('error_log.txt', 'a');

// Escribir mensaje de error en el error estándar
fwrite(STDERR, 'Ha ocurrido un error en el programa.');

// Cerrar el archivo
fclose($file);
```

El código anterior abrirá un archivo llamado `error_log.txt` en modo de escritura y escribirá un mensaje de error en el error estándar utilizando la función `fwrite()`. Una vez que se haya escrito el mensaje, el archivo se cerrará correctamente.

El resultado de este código puede ser algo similar a:

```
PHP Notice: Ha ocurrido un error en el programa. in /Users/Usuario/Documents/programa.php on line 4
```

## Profundizando

Escribir al error estándar en PHP puede ser muy útil tanto para pruebas como para la resolución de problemas en programas más complejos. Además de utilizar la función `fwrite()` mencionada anteriormente, también se pueden utilizar otras técnicas para personalizar y mejorar la información que se envía al error estándar.

Por ejemplo, se puede utilizar la función `error_log()` para enviar mensajes de error a diferentes destinos, como un archivo, una dirección de correo electrónico o el registro del sistema. También se puede utilizar la función `set_error_handler()` para personalizar cómo se manejan y muestran los errores en un programa.

## Vea también

- [Documentación de PHP sobre fwrite()](https://www.php.net/manual/es/function.fwrite.php)
- [Escribir al error estándar en PHP](https://www.samuelaguilera.com/post/escribir-al-error-estandar-en-php/)
- [Personalizar el manejo de errores en PHP](https://www.php.net/manual/es/function.set-error-handler.php)