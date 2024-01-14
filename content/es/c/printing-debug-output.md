---
title:    "C: Imprimiendo salida de depuración"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un programador principiante o incluso si ya llevas tiempo en el mundo de la programación, probablemente te hayas encontrado con el término "imprimir salida de depuración" o "printing debug output". Esto puede sonar un poco técnico e incluso abrumador, pero en realidad es una herramienta muy útil para mejorar tus habilidades de programación. En pocas palabras, imprimir salida de depuración es una forma de mostrar información en la consola o terminal durante el proceso de ejecución de tu programa. Esto puede ayudarte a entender mejor lo que está sucediendo dentro de tu código y encontrar posibles errores.

## Cómo hacerlo

La forma más común de imprimir salida de depuración es utilizando la función `printf()` en lenguaje de programación C. Esta función toma dos argumentos: un formato en el que quieres que se muestre la información (como una cadena de texto con ciertos símbolos de formato) y los valores que quieres mostrar. Por ejemplo, si quieres imprimir el valor de una variable `x`, puedes usar `printf("El valor de x es: %d", x);` Ten en cuenta que en este caso `%d` indica que se imprimirá un número entero.

A continuación, te mostramos un ejemplo de cómo imprimir salida de depuración en un programa sencillo que calcula el área de un triángulo:

```C
#include <stdio.h>

int main()
{
  float base = 5;
  float altura = 3;
  float area = (base * altura) / 2;
  
  printf("La base del triángulo es: %f\n", base);
  printf("Su altura es: %f\n", altura);
  printf("Por lo tanto, el área es: %f\n", area);
  
  return 0;
}
```

Al ejecutar este programa, deberías ver lo siguiente en la consola:

```
La base del triángulo es: 5.000000
Su altura es: 3.000000
Por lo tanto, el área es: 7.500000
```

Como puedes ver, la salida de depuración nos muestra los valores de las variables `base`, `altura` y `area` en cada paso del proceso. Esto puede ser muy útil cuando estás tratando de corregir errores en tu programa o simplemente quieres verificar que tus cálculos sean correctos.

## Profundizando

Ahora que sabes cómo imprimir salida de depuración con `printf()`, es importante mencionar que también hay otras formas de hacerlo en lenguajes de programación como Java o Python. Por ejemplo, en Java puedes utilizar el método `println()` de la clase `System`, mientras que en Python puedes usar la función integrada `print()`. Cada lenguaje tiene su propia forma de imprimir salida de depuración, así que asegúrate de consultar la documentación correspondiente a tu lenguaje de elección.

También es importante mencionar que imprimir demasiada salida de depuración puede ralentizar tu programa y hacerlo menos eficiente. Por lo tanto, es importante utilizarlo de manera estratégica y en áreas donde realmente lo necesites.

## Ver también

- [Debugging for beginners: 10 tips to get you started](https://www.freecodecamp.org/news/debugging-for-beginners-10-tips-to-get-you-started/)
- [The Power of Debugging: An Introduction to Print Statements](https://www.digitalocean.com/community/tutorials/the-power-of-debugging-an-introduction-to-print-statements)