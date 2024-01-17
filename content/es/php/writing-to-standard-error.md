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

## ¿Qué y Por Qué?
La escritura al error estándar es una técnica utilizada por los programadores para enviar mensajes de error a la consola durante la ejecución de un programa. Esto permite a los desarrolladores identificar y solucionar problemas mientras están trabajando en su código. También ayuda a mejorar la experiencia del usuario al proporcionar información detallada sobre cualquier error que pueda ocurrir.

## Cómo hacerlo:
Para escribir al error estándar en PHP, se puede utilizar la función "fwrite()" con el argumento "STDERR" para enviar un mensaje a la consola. A continuación, se presenta un ejemplo de código y su salida correspondiente:
```
<?php
fwrite(STDERR, "¡Este es un mensaje de error!");
?>
```
Salida:
```
¡Este es un mensaje de error!
```

## Inmersión Profunda:
La escritura al error estándar es una técnica ampliamente utilizada en la programación, y su origen se remonta a los inicios de UNIX en los años 70. Sin embargo, con el avance de la tecnología, han surgido nuevas alternativas, como el registro de errores en archivos o el uso de herramientas de depuración.

La implementación de la escritura al error estándar puede variar según el lenguaje de programación utilizado. En PHP, por ejemplo, también se puede utilizar la función "error_log()" para enviar mensajes de error a un archivo en lugar de la consola.

## Ver también:
- Documentación de PHP sobre fwrite(): https://www.php.net/manual/es/function.fwrite.php
- Artículo sobre la importancia de manejar errores de manera adecuada: https://www.php.net/manual/es/language.exceptions.php