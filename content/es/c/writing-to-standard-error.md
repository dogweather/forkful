---
title:    "C: Escribiendo en el error estándar"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por qué

En la programación, a menudo nos encontramos con situaciones en las que necesitamos imprimir mensajes de error para poder depurar nuestro código y detectar posibles errores. Una forma común de hacerlo es escribiendo a la salida estándar, también conocida como "standard output". Sin embargo, también existe la posibilidad de escribir a la salida de error, conocida como "standard error". En este artículo, te explicaremos por qué puede ser útil escribir a la salida de error y cómo hacerlo en lenguaje C.

## Cómo hacerlo

Escribir a la salida de error en C es relativamente sencillo. Todo lo que necesitas hacer es utilizar la función `fprintf()` y pasar como primer argumento el puntero a la salida de error `stderr` en lugar de `stdout`. Veamos un ejemplo de cómo se vería esto en código:

```C
fprintf(stderr, "Este es un mensaje de error\n");
```

En este ejemplo, estamos imprimiendo el mensaje "Este es un mensaje de error" a la salida de error utilizando la función `fprintf()` y el puntero `stderr`. A continuación, podemos ver cómo se vería la salida de este código en la terminal:

```
Este es un mensaje de error
```

Podemos ver que el mensaje se imprime en la línea de comandos, pero ¿por qué deberíamos usar la salida de error en lugar de la salida estándar?

## Profundizando

La principal ventaja de escribir a la salida de error es que estos mensajes se imprimen inmediatamente, mientras que los mensajes de la salida estándar pueden ser almacenados en un buffer antes de ser impresos, lo que puede llevar a problemas de sincronización en el orden de los mensajes. Además, al escribir a la salida de error, podemos diferenciar fácilmente entre mensajes de error y mensajes regulares, lo que puede ser de gran ayuda durante la depuración de nuestro código.

Otra ventaja es que los mensajes de error se mostrarán incluso si redirigimos la salida estándar a un archivo. Esto significa que podremos ver los mensajes de error en la terminal, mientras que la salida estándar se almacenará en el archivo. Esto puede ser muy útil si estamos ejecutando nuestro programa en segundo plano y queremos revisar los errores más tarde.

También es importante tener en cuenta que al escribir a la salida de error, podemos utilizar caracteres de formato como en `printf()`, lo que nos permite mostrar información detallada sobre el error que estamos reportando.

## Ver también

- [Función de impresión y formato fprintf() en C](https://www.programiz.com/c-programming/library-function/stdio.h/fprintf)
- [Salida estándar vs salida de error en C](https://www.theunixschool.com/2012/07/standard-output-vs-standard-error-in-c.html)
- [Cómo redirigir la salida y el error en C](https://stackoverflow.com/questions/13153760/redirecting-stdout-stderr-and-stdin-to-a-file)

Esperamos que este artículo te haya sido útil y que ahora tengas una comprensión más clara de cómo escribir a la salida de error en tus programas en C. ¡Gracias por leer!