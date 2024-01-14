---
title:                "C: Escribir en el error estándar"
simple_title:         "Escribir en el error estándar"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

Muchas veces cuando codificamos en C, es posible que nos encontremos con errores que nos impiden que nuestro programa funcione correctamente. Al escribir a la salida estándar, estos errores se pueden mostrar en la pantalla, pero ¿qué pasa si queremos ver los errores en un archivo de registro? Esta es la razón por la que escribir a la salida de error es muy importante en la programación en C.

## Cómo hacerlo

Para escribir a la salida de error en C, utilizamos la función `fprintf()` y especificamos la salida estándar de error con el identificador `stderr`. Veamos un ejemplo:

```C
fprintf(stderr, "Este es un error: %d\n", error_code);
```

En este código, estamos usando `fprintf()` para escribir a la salida de error y estamos mostrando el valor de la variable `error_code` con el formato `%d`, que en este caso es un número entero.

Ahora, para ver el resultado de este código, necesitamos ejecutar nuestro programa en la línea de comandos y redirigir la salida de error a un archivo de registro. Esto se hace añadiendo `2>` al final del comando de ejecución, junto con el nombre del archivo de registro. Por ejemplo:

```bash
./programa 2> registro.txt
```

Esto enviará todos los errores a un archivo llamado "registro.txt" en lugar de mostrarlos en la pantalla.

## Profundizando

Cuando escribimos a la salida de error en C, es importante tener en cuenta que no se debe confundir con la salida estándar. Es necesario especificar específicamente `stderr` para asegurarse de que los errores se envíen a la salida de error en lugar de a la salida estándar.

También es importante recordar que cuando se redirige la salida de error a un archivo de registro, esto solo afecta a los errores. La salida estándar seguirá mostrando los resultados en la pantalla. Por lo tanto, es posible que aún necesite usar `printf()` para mostrar información importante en la pantalla para el usuario.

En resumen, escribir a la salida de error en C nos permite registrar y controlar los errores en nuestro programa de manera más eficiente, lo que es esencial para el desarrollo y depuración de software.

## Ver también

- [Documentación de `fprintf()`(https://www.ibm.com/docs/en/zos/2.2.0?topic=functions-fprintf-write-formatted-data-stream) - para obtener más información sobre cómo utilizar esta función en C.
- [Tutorial de redirección de salida estándar y estándar de error](https://www.cyberciti.biz/faq/redirecting-stderr-to-stdout/) - si desea aprender más sobre la redirección de la salida en la línea de comandos.
- [Depuración de programas en C en Linux](https://www.thegeekstuff.com/2010/03/debug-c-program-using-gdb/) - para obtener ayuda sobre cómo solucionar los errores en su código.