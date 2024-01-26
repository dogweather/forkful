---
title:                "Escribiendo en el error estándar"
html_title:           "Arduino: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir en "standard error" (stderr) permite a los programas comunicar errores. Los programadores lo utilizan para separar la salida normal de la información de errores, facilitando el diagnóstico y el manejo de excepciones.

## Cómo hacerlo:

En PHP, puedes escribir en stderr usando `fwrite` y `STDERR` o con funciones como `error_log`. Aquí unos ejemplos:

```php
<?php
// Escribir en stderr usando fwrite.
fwrite(STDERR, "Este es un mensaje de error.\n");

// Usando error_log con el parámetro 4 para enviar el mensaje a stderr.
error_log("Otro mensaje de error.", 4);
?>
```

Si ejecutas esto, no verás los mensajes en el navegador, pero aparecerán en la consola o en los logs de error.

## Profundizando:

Historicamente, stderr es uno de los tres flujos de datos originales en los sistemas Unix, junto con stdin y stdout. En PHP, `STDERR` es una constante predefinida que solo está disponible cuando PHP se ejecuta en un modo que no es CGI o servidor web. Como alternativas, podrías escribir en un archivo de log personalizado o utilizar librerías para manejar errores. La implementación de stderr en PHP es parte de las I/O streams, que manejan la entrada y salida de datos.

## Véase también:

- Documentación oficial de PHP sobre manejo de errores: https://www.php.net/manual/es/book.errorfunc.php
- I/O streams en PHP: https://www.php.net/manual/es/wrappers.php.php
- Flujos de datos en Unix: https://es.wikipedia.org/wiki/Unix_stream
