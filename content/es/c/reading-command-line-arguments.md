---
title:    "C: La lectura de argumentos de línea de comandos"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

Si estás leyendo esto, es probable que estés buscando cómo leer argumentos de línea de comandos en tus programas de C. Puede parecer una tarea tediosa y poco útil, pero en realidad es una habilidad muy valiosa para cualquier programador serio.

## Cómo hacerlo

Para leer argumentos de línea de comandos en C, necesitamos utilizar la función `main()`, ya que es el punto de entrada de nuestro programa. La firma de esta función es la siguiente:

```C
int main(int argc, char *argv[])
```

Donde `argc` es un entero que representa el número de argumentos de línea de comandos y `argv` es un array de strings que contienen los argumentos en sí. Ahora, veamos un ejemplo de cómo podemos utilizar esto para mostrar los argumentos ingresados por el usuario:

```C
#include <stdio.h>

int main(int argc, char *argv[])
{
    int i;

    printf("Los argumentos de la línea de comandos son:\n");

    for (i = 0; i < argc; i++) {
        printf("%s\n", argv[i]);
    }

    return 0;
}
```

Si compilamos y ejecutamos este programa con los argumentos "hola mundo", obtendremos la siguiente salida:

```
Los argumentos de la línea de comandos son:
./programa
hola
mundo
```

Como podemos ver, el primer argumento siempre es el nombre del programa en sí.

## Buceo profundo

Además de los argumentos ingresados por el usuario, también tenemos la opción de proporcionar argumentos predeterminados cuando ejecutamos nuestro programa desde el terminal. Esto se puede hacer añadiendo argumentos después del nombre del programa, separados por un espacio. Por ejemplo, si ejecutamos `./programa arg1 arg2`, `argc` será igual a 3 y `argv` contendrá los strings "arg1" y "arg2" además del nombre del programa.

También es importante tener en cuenta que los argumentos de línea de comandos se pasan como strings, lo que significa que si queremos trabajar con números, tendremos que convertirlos mediante funciones como `atoi()` o `strtol()`.

Además, aunque la mayoría de las veces solo necesitamos leer argumentos desde la línea de comandos, también podemos escribir en ellos utilizando la función `sprintf()`, por ejemplo.

## Ver también

- [Documentación de la función `main()` en C](https://www.tutorialspoint.com/cprogramming/c_main_function.htm)
- [Convertir strings a enteros en C](https://www.geeksforgeeks.org/converting-strings-numbers-cc/)
- [Cómo utilizar la función `sprintf()` en C](https://www.includehelp.com/c-programs/sprintf-function-in-c-programming.aspx)