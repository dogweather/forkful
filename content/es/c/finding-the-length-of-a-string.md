---
title:    "C: Encontrar la longitud de una cadena"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por qué

¿Te has preguntado alguna vez cómo calcular la longitud de una cadena de texto en C? Puede parecer una pregunta simple, pero saber hacerlo puede ser muy útil en el desarrollo de programas. En este artículo, te explicaré por qué es importante saber cómo encontrar la longitud de una cadena y cómo puedes hacerlo de manera efectiva.

## Cómo hacerlo

En C, una cadena de texto es simplemente una secuencia de caracteres delimitada por un valor especial, que es el caracter nulo. Pero encontrar la longitud de una cadena de texto no es tan sencillo como contar los caracteres. La función `strlen()` de la librería estándar de C puede ayudarte a obtener la longitud de una cadena. Aquí te dejo un ejemplo de cómo usarla:

```C
#include<stdio.h>
#include<string.h>

int main() {
  char cadena[] = "Hola mundo";
  int longitud = strlen(cadena);

  printf("La longitud de la cadena es %d\n", longitud);

  return 0;
}
```

En este ejemplo, estamos utilizando la función `strlen()` para calcular la longitud de la cadena `"Hola mundo"` y almacenar el resultado en la variable `longitud`. Luego, imprimimos en pantalla el resultado usando la función `printf()`.

La salida de este programa sería:

```
La longitud de la cadena es 10
```

## Profundizando

Ahora que ya sabes cómo usar la función `strlen()` para encontrar la longitud de una cadena de texto en C, es importante entender cómo funciona. Esta función recibe como parámetro una cadena de texto y recorre cada uno de sus caracteres hasta encontrar el valor nulo que indica el final de la cadena. Cada vez que encuentra un caracter, incrementa en 1 el valor de una variable interna que almacena la longitud de la cadena. Una vez que llega al caracter nulo, devuelve el valor de esa variable.

Es importante tener en cuenta que la longitud de una cadena en C incluye todos los caracteres, incluyendo los espacios y los caracteres especiales. Además, si la cadena no está terminada con el valor nulo, la función `strlen()` podría seguir contando caracteres hasta que encuentra uno dentro de la memoria asignada a la variable. Esto podría causar comportamientos inesperados en tu programa.

## Ver también

- [Documentación de la función `strlen()` en la biblioteca estándar de C](https://www.cplusplus.com/reference/cstring/strlen/)
- [Cadenas de texto en C - Tutorial básico](https://www.programiz.com/c-programming/c-strings)

¡Espero que este artículo te haya sido útil! Ahora ya sabes cómo encontrar la longitud de una cadena de texto en C. ¡Pero no te detengas aquí! Sigue explorando y aprendiendo nuevas funciones y técnicas de programación en C. ¡Hasta la próxima!