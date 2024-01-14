---
title:                "C: Leyendo un archivo de texto"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

La lectura de archivos de texto es una tarea fundamental para cualquier programador de C. Puede ser útil para leer información almacenada en archivos, como datos de entrada o de salida, o también para manipular y modificar el contenido del archivo. En este post, te guiaremos a través de los fundamentos de cómo leer un archivo de texto en C y cómo utilizarlo en tus programas.

## Cómo hacerlo

Para leer un archivo de texto en C, necesitamos seguir algunos pasos básicos:

1. Abrir el archivo para su lectura utilizando la función `fopen()`. Esta función toma dos argumentos: la ruta del archivo que deseamos leer y el modo en el que deseamos abrirlo (en este caso, "r" para lectura).

2. Comprobar si se pudo abrir el archivo correctamente. Si no se pudo abrir, `fopen()` devolverá `NULL` y podemos imprimir un mensaje de error.

3. Leer el contenido del archivo utilizando la función `fgets()`. Esta función toma tres argumentos: un puntero al búfer donde se almacenará el texto leído, el número máximo de caracteres a leer y un puntero al archivo que estamos leyendo. `fgets()` devolverá `NULL` cuando alcance el final del archivo.

4. Procesar y utilizar el texto leído. En este ejemplo, simplemente imprimimos el texto en la pantalla.

5. Cerrar el archivo utilizando `fclose()` cuando hayamos terminado de leer.

A continuación, se presenta un ejemplo de código que implementa estos pasos:

```
#include <stdio.h>

int main() {
    FILE *archivo;
    char linea[100];

    // Abrir el archivo
    archivo = fopen("archivo.txt", "r");

    // Comprobar si se pudo abrir
    if (archivo == NULL) {
        printf("No se pudo abrir el archivo");
        return 1;
    }

    // Leer líneas hasta el final del archivo
    while (fgets(linea, 100, archivo) != NULL) {
        // Imprimir el contenido del archivo
        printf("%s", linea);
    }

    // Cerrar el archivo
    fclose(archivo);
    return 0;
}
```

Si suponemos que tenemos un archivo de texto llamado `archivo.txt` con el siguiente contenido:

```
Bienvenido al blog de programación.
Hoy vamos a hablar sobre cómo leer archivos de texto en C.
¡Comencemos!
```

El programa anterior producirá la siguiente salida en la pantalla:

```
Bienvenido al blog de programación.
Hoy vamos a hablar sobre cómo leer archivos de texto en C.
¡Comencemos!
```

## Profundizando más

En el ejemplo anterior, hemos utilizado la función `fgets()` para leer líneas completas del archivo. Sin embargo, existen otras funciones que nos permiten leer de manera diferente, como `fgetc()` para leer caracteres individuales o `fscanf()` para leer datos formateados.

Además, es importante tener en cuenta que al final de cada línea de texto en un archivo de texto hay un carácter de nueva línea (`\n`). Esto significa que si utilizamos `fgets()` para leer una línea, se incluirá el carácter de nueva línea en el búfer. Para evitar esto, podemos utilizar `sscanf()` para leer el contenido sin incluir el carácter de nueva línea.

También es posible escribir en un archivo de texto utilizando las funciones `fprintf()` y `fputs()`.

En resumen, la manipulación de archivos de texto es una habilidad importante para cualquier programador de C y hay muchas funciones disponibles para ayudarnos a realizar esta tarea de manera eficiente y efectiva.

## Ver también

- [Manipulación de archivos en C](https://www.programiz.com/c-programming/c-file-input-output)
- [Ejemplos de lectura y escritura de archivos en C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [Documentación oficial de C sobre la biblioteca estándar para manejo de archivos](https://www.gnu.org/software/make/manual/html_node/Standard-Options.html#Standard-Options)