---
title:    "C: Leyendo argumentos de línea de comandos."
keywords: ["C"]
---

{{< edit_this_page >}}

# ¿Por qué leer argumentos de línea de comando en C?

Si eres un programador en C, es posible que hayas oído hablar de los argumentos de línea de comando, pero ¿sabes realmente por qué son importantes? En este artículo, exploraremos los argumentos de línea de comando y por qué es valioso para los programadores conocer cómo leerlos.

## Cómo leer argumentos de línea de comando en C

En C, hay dos formas básicas de recibir datos del usuario: desde el teclado y desde argumentos de línea de comando. Para leer argumentos de línea de comando, se utilizan los parámetros de la función `main()`. Estos parámetros son `argc`, que corresponde al número de argumentos, y `argv`, que es un array que almacena los argumentos.

Veamos un ejemplo de cómo leer argumentos de línea de comando y mostrarlos en la pantalla:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {

    // Iterar a través de los argumentos y mostrarlos en la pantalla
    for(int i = 0; i < argc; i++) {
        printf("Argumento %d: %s\n", i, argv[i]);
    }

    return 0;
}
```

Si compilamos y ejecutamos este código con los siguientes argumentos: `./arguments hola mundo`, obtendremos la siguiente salida:

```
Argument 0: ./arguments
Argument 1: hola
Argument 2: mundo
```

En este ejemplo, `argc` será igual a 3 (hay tres argumentos) y `argv` almacenará los argumentos `hola` y `mundo` en las posiciones 1 y 2.

## Profundizando en la lectura de argumentos de línea de comando en C

Además de leer los argumentos y mostrarlos en la pantalla, es posible realizar acciones más complejas con ellos. Por ejemplo, podrías utilizar un argumento para determinar qué acción debe realizar tu programa, o incluso utilizarlos para leer un archivo externo.

También es importante tener en cuenta las rutinas de comprobación de errores al leer argumentos de línea de comando. Si no se proporcionan suficientes argumentos o si se ingresan argumentos inválidos, tu programa podría terminar con un error. Por lo tanto, es importante implementar una lógica de control para evitar estos problemas.

# Ver también

- [Documentación oficial de argumentos de línea de comando en C](https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html)
- [Tutorial de programación en C para principiantes](https://www.programiz.com/c-programming)
- [Introducción a la línea de comando de Linux para programadores](https://www.digitalocean.com/community/tutorials/an-introduction-to-the-linux-terminal)