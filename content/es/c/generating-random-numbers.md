---
title:                "C: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué generar números aleatorios en programación

La generación de números aleatorios es una herramienta útil en la programación para simular situaciones de incertidumbre o tomar decisiones al azar. Puede ser utilizado en una variedad de aplicaciones, como juegos, criptografía y pruebas de algoritmos.

## Cómo hacerlo

Para generar números aleatorios en C, utilizamos la función `rand()` de la biblioteca estándar `stdlib.h`. Esta función devuelve un número entero pseudorandom en el rango de 0 a `RAND_MAX`, que es al menos 32767. Antes de utilizar la función `rand()`, debemos llamar a la función `srand()` para inicializar la semilla del generador de números aleatorios.

Veamos un ejemplo de cómo generar 10 números aleatorios entre 1 y 100 y mostrarlos en pantalla:

```C
#include <stdio.h>
#include <stdlib.h>

int main(){
  int i;
  srand(time(NULL)); //inicializar la semilla utilizando el tiempo actual
  for(i=0; i<10; i++){
    int num = rand() % 100 + 1; //generamos un número pseudoaleatorio entre 1 y 100
    printf("%d ", num);
  }
  return 0;
}
```

La salida de este código puede ser, por ejemplo: `47 92 10 68 33 86 59 56 74 91`.

## Profundizando

Es importante tener en cuenta que los números generados por esta función no son realmente aleatorios, ya que siguen un patrón predecible. Esto se debe a que utilizan una fórmula matemática para producir los números, por lo que se les denominan "pseudoaleatorios". Sin embargo, para la mayoría de las aplicaciones, esta función es suficientemente aleatoria.

Si queremos generar números en un rango específico más grande, podemos utilizar la fórmula `rand() % (max - min + 1) + min` en lugar de `rand() % n + 1` para generar números entre `min` y `max`.

También es posible utilizar la función `srand()` con una semilla constante para obtener la misma secuencia de números pseudoaleatorios en cada ejecución del programa. Esto puede ser útil para depurar o probar un programa.

## Ver también

- [Documentación de la función rand() en C](https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm)
- [Explicación de números pseudoaleatorios y semillas en programación](https://www.geeksforgeeks.org/pseudo-random-number-generator-prng/)
- [Ejemplo de generación de números aleatorios en diferentes lenguajes de programación](https://rosettacode.org/wiki/Random_numbers)