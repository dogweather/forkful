---
title:                "C: Impresión de salida de depuración."
simple_title:         "Impresión de salida de depuración."
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Por qué imprimir la salida de depuración es importante en programación?

Para cualquier programador, es esencial entender el funcionamiento interno de su código y poder detectar errores. Imprimir la salida de depuración es una técnica útil para identificar fallos en el código y facilitar el proceso de corrección. Además, puede ayudar a entender mejor el flujo de ejecución y optimizar el rendimiento de la aplicación.

## Cómo imprimir la salida de depuración en C

Imprimir la salida de depuración en C es muy sencillo. Simplemente se utiliza la función `printf()` para mostrar mensajes en la consola. Veamos un ejemplo:

```C
#include <stdio.h>

int main() {
  int num1 = 10;
  int num2 = 5;
  
  printf("El valor de num1 es %d y el de num2 es %d\n", num1, num2);
  
  return 0;
}
```

En este ejemplo, utilizamos la función `printf()` para mostrar los valores de las variables `num1` y `num2` en la consola. Al compilar y ejecutar el programa, obtendremos la siguiente salida:

```
El valor de num1 es 10 y el de num2 es 5
```

De esta manera, podemos ver fácilmente los valores de las variables en un determinado punto del programa.

## Más información sobre imprimir la salida de depuración

Además de utilizar `printf()`, también podemos imprimir la salida de depuración a través de otros medios, como por ejemplo, un archivo de texto. Para hacer esto, se utiliza la función `fprintf()` en lugar de `printf()`.

Además, también podemos imprimir mensajes de error utilizando la función `perror()`, lo que puede ser útil para detectar errores en tiempo de ejecución y facilitar su corrección.

Es importante destacar que imprimir demasiada salida de depuración puede afectar el rendimiento de la aplicación, por lo que se recomienda utilizarla solo cuando sea necesario y eliminarla después de solucionar los errores.

## Ver también

- [Documentación de printf en C](https://www.codingame.com/playgrounds/4242/funciones-y-punteros-en-c/printf)
- [Tutorial de depuración en C](https://www.learn-c.org/en/Debugging)
- [Cómo utilizar GDB para depurar programas en C](https://gdbgui.com/tutorials/debug-c-in-visual-studio-code)