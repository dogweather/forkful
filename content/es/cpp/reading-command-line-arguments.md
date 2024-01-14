---
title:    "C++: Leyendo argumentos de línea de comandos"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Por qué leer argumentos de línea de comandos en C++?

La lectura de argumentos de línea de comandos es una habilidad esencial para cualquier programador en C++. Permite que el usuario proporcione información al programa cuando se ejecuta, lo cual es muy útil en casos como la personalización de opciones o la interacción con otras aplicaciones.

## Cómo leer argumentos de línea de comandos en C++

En C++, la función `main` toma dos parámetros: `argc` y `argv`. `argc` es el número de argumentos ingresados por el usuario, mientras que `argv` es un arreglo de cadenas con los argumentos. A continuación, se muestra un ejemplo de cómo leer y mostrar los argumentos de línea de comandos en C++:

```C++
#include <iostream>

int main(int argc, char* argv[]) {
    std::cout << "Se han ingresado " << argc << " argumentos:" << std::endl;
    for (int i = 0; i < argc; i++) {
        std::cout << "Argumento " << i + 1 << ": " << argv[i] << std::endl;
    }
    return 0;
}
```

Si ejecutamos este programa con los argumentos `hola mundo`, obtendremos la siguiente salida:

```
Se han ingresado 3 argumentos:
Argumento 1: ./programa
Argumento 2: hola
Argumento 3: mundo
```

## Profundizando en la lectura de argumentos de línea de comandos

Además de la función `main`, existen otras formas de leer argumentos de línea de comandos en C++. Una de ellas es mediante el uso de la librería `getopt`, que permite definir opciones y argumentos en un formato más estructurado.

Por ejemplo, podríamos tener un programa que acepte las opciones `-h` para mostrar ayuda y `-n` para especificar un nombre, junto con el argumento obligatorio del nombre de la persona. A continuación, se muestra un ejemplo de cómo utilizar `getopt` para lograr esto:

```C++
#include <iostream>
#include <unistd.h>

int main(int argc, char *argv[]) {
    int opt;
    bool showHelp = false;
    char* name;
    
    // Definir las opciones y argumentos esperados
    const char* optString = "hn:";
    // Iterar hasta haber procesado todos los argumentos
    while ((opt = getopt(argc, argv, optString)) != -1) {
        switch(opt) {
            case 'h':
                // Si se ingresa "-h", se activa el flag
                showHelp = true;
                break;
            case 'n':
                // Si se ingresa "-n", se obtiene el argumento después de la opción
                name = optarg;
                break;
        }
    }
    // Imprimir la ayuda si se pasó la opción "-h" o si no se especificó un nombre
    if (showHelp || name == NULL) {
        std::cout << "Uso: programa -n <nombre>" << std::endl;
        return 0;
    }
    std::cout << "¡Hola, " << name << "!" << std::endl;
    return 0;
}
```

Si ejecutamos este programa con los argumentos `-n María`, obtendremos la salida `¡Hola, María!`.

## Ver también

- [Documentación sobre la función `main` en C++](https://www.cplusplus.com/reference/cstdlib/main/)
- [Documentación sobre la librería `getopt` en C++](https://www.gnu.org/software/libc/manual/html_node/Getopt.html)
- [Ejemplos de uso de la función `main` y `getopt`](https://www.tutorialspoint.com/cplusplus/cpp_command_line_arguments.htm)