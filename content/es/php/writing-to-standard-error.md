---
title:                "PHP: Escribiendo a la salida de error estándar"
simple_title:         "Escribiendo a la salida de error estándar"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué escribir a la salida de error estándar?

Escribir a la salida de error estándar (STDERR) es un proceso común en la programación PHP. Se utiliza para imprimir mensajes de error al usuario durante la ejecución del programa. Al usar esta técnica, los programadores pueden identificar y solucionar errores más fácilmente, mejorando la calidad de su código.

##Cómo hacerlo:

Usar la función `fwrite()` es la forma más común de escribir a la salida de error estándar en PHP. Primero, debes abrir la salida de error estándar con la función `fopen()` y especificar que quieres escribir usando el modo "a" (append). Luego, puedes usar la función `fwrite()` para escribir tu mensaje de error en la salida de error estándar. Finalmente, asegúrate de cerrar la salida de error estándar con la función `fclose()`.

```PHP
$stdout = fopen('php://stdout', 'a');
fwrite($stdout, "Este es un mensaje de error");
fclose($stdout);
```

Este código imprimirá el mensaje de error en la pantalla del usuario mientras se ejecuta el programa.

## Deep Dive:

Cuando se escribe a la salida de error estándar, también es importante tener en cuenta la implementación del proceso de manejo de errores en tu código. Puedes usar la función `set_error_handler()` para personalizar cómo se manejan los errores en tu programa. También puedes usar la función `error_reporting()` para controlar el nivel de detalle de los errores que se muestran al usuario.

Otra cosa importante a tener en cuenta es que la salida de error estándar no solo se limita a mensajes de error. También se puede usar para mostrar información importante o de depuración durante la ejecución del programa. Esto puede ser útil para identificar y corregir errores en tu código.

## Ver también:

- [Documentación oficial de PHP sobre la función fwrite()](https://www.php.net/manual/en/function.fwrite.php)
- [Artículo sobre la gestión de errores en PHP](https://www.php.net/manual/en/language.exceptions.php)
- [Tutorial sobre el manejo de errores en PHP](https://www.w3schools.com/php/php_error.asp)