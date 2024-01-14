---
title:    "PHP: Escritura en el error estándar"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por qué

A veces, cuando escribimos código en PHP, es útil tener una manera de imprimir mensajes de error. Esto nos permite identificar rápidamente problemas en nuestro código y solucionarlos.

## Cómo hacerlo

Para imprimir mensajes de error en PHP, podemos usar la función `error_log()`. Esta función toma dos parámetros: el mensaje que queremos imprimir y el tipo de error. Por ejemplo:

```
PHP error_log("¡Hola! Esto es un mensaje de error.", 3);
```

El número `3` en el segundo parámetro indica que este es un mensaje de error y se enviará al registro de errores. Si utilizamos `0` en su lugar, el mensaje se imprimirá en la salida estándar. También podemos enviar el mensaje a un archivo específico proporcionando su ruta en lugar del número.

Ahora, si ejecutamos nuestro código, veremos nuestro mensaje de error impreso en el lugar que hayamos especificado.

## Profundizando

Además de imprimir mensajes de error en el registro de errores, también es posible enviarlos al navegador. Podemos hacer esto enviando un encabezado del tipo `text/plain` antes del mensaje de error, usando la función `header()`.

Otra opción es enviar el mensaje de error por correo electrónico. Esto puede ser útil en entornos de producción para recibir notificaciones sobre posibles problemas en nuestro código.

También es importante tener en cuenta que, para imprimir mensajes de error, es necesario que el registro de errores esté habilitado en la configuración de PHP. De lo contrario, nuestros mensajes no se imprimirán.

## Ver También

- [Manual de PHP: error_log()](https://www.php.net/manual/es/function.error-log.php)
- [Manual de PHP: header()](https://www.php.net/manual/es/function.header.php)
- [Configuración del registro de errores en PHP](https://www.php.net/manual/es/errorfunc.configuration.php#ini.error-log)