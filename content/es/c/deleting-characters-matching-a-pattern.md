---
title:    "C: Borrando caracteres que coinciden con un patrón."
keywords: ["C"]
---

{{< edit_this_page >}}

## Por qué

A veces, en la programación, es necesario eliminar caracteres de un texto que cumplan con ciertos patrones. Ya sea para limpiar datos, filtrar información o para otras tareas, la eliminación de estos caracteres puede ser una tarea común en la programación en C. En este artículo, exploraremos cómo eliminar caracteres que coinciden con un patrón específico y por qué puede ser útil hacerlo.

## Cómo hacerlo

Eliminación de caracteres que coinciden con un patrón en C es relativamente sencillo. Primero, debes declarar una cadena de entrada que contenga los caracteres que deseas analizar. Luego, utilizando un bucle, puedes recorrer cada carácter de la cadena y verificar si coincide con el patrón deseado. Si es así, simplemente salta ese carácter sin incluirlo en la cadena de salida. Aquí hay un ejemplo de código:

```C
#include <stdio.h>

int main() {
  char input[] = "ab@cde^fg*h";
  char output[256] = {0};
  int input_length = 0;
  int output_length = 0;

  while (input[input_length]) {
    if (input[input_length] != '@' && input[input_length] != '^') {
      output[output_length] = input[input_length];
      output_length++;
    }
    input_length++;
  }

  printf("Output: %s\n", output);

  return 0;
}
```

Este código eliminará todos los caracteres '@' y '^' del texto de entrada, y el resultado será:

```bash
Output: abcdefg*h
```

Puedes adaptar este código para que elimine cualquier patrón que necesites, simplemente cambiando los caracteres del `if` por los que deseas eliminar.

## Profundizando

En este ejemplo, estamos utilizando un enfoque basado en la comparación de caracteres para eliminar los que coinciden con el patrón. Sin embargo, hay otras técnicas que también se pueden utilizar, como la función `strtok()` o expresiones regulares que pueden hacer el proceso más eficiente y versátil. También puede ser útil crear una función reutilizable para eliminar caracteres coincidentes, en lugar de escribir el mismo código cada vez que necesites hacerlo.

## Ver también
- [Documentación de la función `strtok()` en C](https://www.programiz.com/c-programming/library-function/string.h/strtok)
- [Tutorial de expresiones regulares en C](https://www.tutorialspoint.com/c_standard_library/regex_h.htm)
- [Guía para escribir funciones reutilizables en C](https://www.geeksforgeeks.org/how-to-write-functions-that-modify-their-arguments-in-c/)