---
title:                "Lectura de un archivo de texto"
html_title:           "C: Lectura de un archivo de texto"
simple_title:         "Lectura de un archivo de texto"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

¡Hola lectores! ¿Alguna vez has querido leer datos de un archivo de texto con tu programa en C? ¡Con este artículo, aprenderás cómo hacerlo! Ya sea que necesites leer y procesar datos de un archivo de configuración o de un archivo de registro, leer un archivo de texto te permitirá obtener la información necesaria para que tu programa funcione correctamente. ¡Sigue leyendo para descubrir cómo hacerlo!

## How To

La función `fopen()` en C te permite abrir un archivo en modo lectura, que es lo que necesitamos para leer un archivo de texto. Luego, podemos utilizar la función `fscanf()` para leer el contenido del archivo línea por línea. Aquí hay un ejemplo de cómo podrías implementar esto en tu programa:

```C
#include <stdio.h>

int main() {
  // Abrir archivo en modo lectura
  FILE *archivo = fopen("datos.txt", "r");
  // Verificar si se pudo abrir correctamente
  if (archivo == NULL) {
    printf("Error al abrir el archivo\n");
    return 1;
  }

  // Variables para almacenar los datos leídos
  int num1, num2;
  char texto[100];

  // Leer archivo línea por línea y almacenar datos en variables
  while (fscanf(archivo, "%d %d %[^\n]", &num1, &num2, texto) == 3) {
    // Hacer algo con los datos leídos, como imprimirlos en pantalla
    printf("Número 1: %d, Número 2: %d, Texto: %s\n", num1, num2, texto);
  }

  // Cerrar archivo después de su uso
  fclose(archivo);
  return 0;
}
```

Suponiendo que el archivo `datos.txt` se ve así:

```
1 2 Hola
3 4 Adiós
```

El resultado en la consola sería:

```
Número 1: 1, Número 2: 2, Texto: Hola
Número 1: 3, Número 2: 4, Texto: Adiós
```

## Deep Dive

Además de `fscanf()`, existen otras funciones que pueden ayudarte a leer datos de un archivo de texto en C. Por ejemplo, `fgets()` te permite leer una línea completa del archivo, mientras que `fgetc()` te permite leer un solo caracter a la vez. También hay opciones para posicionarse en una línea específica del archivo o avanzar o retroceder una cierta cantidad de caracteres. ¡Explora la documentación de C para encontrar todas las opciones disponibles!

## See Also

- Documentación oficial de C: https://www.gnu.org/software/libc/manual/html_node/Input-and-Output-on-Streams.html#Input-and-Output-on-Streams
- Ejemplos de lectura de archivos de texto en C: https://www.w3schools.in/c-tutorial/file-input-output/
- Tutorial de C en español: https://www.tutorialspoint.com/cprogramming/