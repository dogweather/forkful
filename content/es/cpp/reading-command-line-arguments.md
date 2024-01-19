---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Qué y Por qué?

Leer argumentos de línea de comandos implica obtener datos de entrada que el usuario provee cuando inicia una aplicación desde su terminal. Esto es útil, ya que nos permite modificar el comportamiento de nuestra aplicación al iniciarla, sin cambiar el código.

# Cómo hacerlo:

Para leer argumentos de la línea de comandos en C++, utilizamos `argc` y `argv[]` proporcionados por la función `main()`. `argc` es la cantidad de argumentos proporcionados y `argv[]` es un array que contiene los argumentos. El primer argumento siempre es el nombre del programa.

```C++
#include<iostream>
int main(int argc, char *argv[]) {
    std::cout << "Número de argumentos: " << argc << "\n";
    for(int i = 0; i < argc; i++) {
        std::cout << "Argumento " << i << ": " << argv[i] << "\n";
    }
    return 0;
}
```

Si ejecutamos el programa con `./programa arg1 arg2`, la salida sería:

```
Número de argumentos: 3
Argumento 0: ./programa
Argumento 1: arg1
Argumento 2: arg2
```

# Profundizando

#### Contexto histórico
Los argumentos de línea de comandos se han utilizado desde los primeros días de la programación en Unix, como una forma eficaz de interactuar con las aplicaciones. Siguen siendo populares en aplicaciones modernas de Unix, Linux y Mac, y también en aplicaciones Windows.

#### Alternativas
En algunos casos, puede ser mejor usar `getopt()` de la biblioteca de C para parsear argumentos, especialmente si se requiere un manejo más complejo, como soporte para argumentos opcionales, flags, etc.

#### Detalles de implementación
`argc` y `argv` son proporcionados por el sistema operativo al programa. `argv[0]` siempre es el nombre del programa, `argv[1]` es el primer argumento proporcionado por el usuario, y así sucesivamente. `argc` es siempre al menos 1.

# Ver también

- [Documentación de C++](http://www.cplusplus.com/)
- [Getopt para parsear argumentos de línea de comandos](http://man7.org/linux/man-pages/man3/getopt.3.html)
- [Stackoverflow - Preguntas sobre argumentos de línea de comandos](https://stackoverflow.com/questions/tagged/command-line-arguments)