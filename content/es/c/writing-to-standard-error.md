---
title:                "C: Escribir a error estándar"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué escribir a la salida de error en C

Escribir a la salida de error en C es una práctica común en la programación, especialmente en aplicaciones grandes y complejas. Esto se debe a que es una forma eficaz de detectar y solucionar errores en el código.

## Cómo hacerlo

Para escribir a la salida de error en C, se utiliza la función `fprintf()` y se le pasa `stderr` como primer argumento. `stderr` es un archivo especial que se utiliza para mostrar mensajes de error. A continuación, se muestra un ejemplo de cómo imprimir un mensaje de error en la salida de error:

```
#include <stdio.h>

int main()
{
    fprintf(stderr, "¡Error! No se pudo abrir el archivo.");
    return 0;
}
```

La función `fprintf()` acepta varios argumentos, pero en este caso, solo se utiliza el argumento de cadena de formato para especificar el mensaje de error. También se puede utilizar `printf()` para escribir a la salida de error, pero se recomienda utilizar `fprintf()` ya que está específicamente diseñado para escribir en archivos.

La salida de error se mostrará en la consola en caso de que se ejecute el programa desde la línea de comandos. En caso contrario, el mensaje de error se guardará en un archivo de registro.

## Profundizando

Además de imprimir mensajes de error, también se pueden utilizar los códigos de retorno para indicar el tipo de error que se ha producido. Por ejemplo, se puede utilizar el código de retorno 1 para indicar que no se pudo abrir un archivo y el código de retorno 2 para indicar que se ha producido un error en la lectura del archivo.

También se pueden utilizar estructuras de control para imprimir mensajes de error específicos en función del tipo de error detectado. Por ejemplo, se puede utilizar la estructura `if-else` para imprimir un mensaje de error diferente si se produce un error de lectura o un error de escritura en un archivo.

## Ver también

- Documentación de fprintf: https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm
- Códigos de retorno: https://www.tutorialspoint.com/cprogramming/c_functions_exit.htm