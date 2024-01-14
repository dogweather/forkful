---
title:    "C: Generando números aleatorios"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué generar números aleatorios en programación

La generación de números aleatorios es una técnica muy útil en programación ya que permite generar valores que no están predefinidos, proporcionando una mayor variedad de datos a utilizar en nuestros programas. Esto puede ser especialmente útil en juegos, algoritmos de inteligencia artificial y pruebas de software.

## Cómo generar números aleatorios en C

En el lenguaje de programación C, contamos con la función `rand()` para generar números aleatorios. Esta función toma como argumento un número entero que representa la "semilla" para la generación de números aleatorios. A continuación, se presenta un ejemplo de código que genera 5 números aleatorios entre 1 y 100 y los muestra por pantalla:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
    // Se utiliza el tiempo actual como semilla
    srand(time(NULL)); 
    int i;
    // Se generan y muestran 5 números aleatorios
    for(i = 0; i < 5; i++)
        printf("%d\n", rand() % 100 + 1);
    return 0;
}
```

**Salida de ejemplo:**

```
72
89
17
56
94
```

## Profundizando en la generación de números aleatorios

Es importante tener en cuenta que los números generados por la función `rand()` no son realmente aleatorios, sino que siguen un patrón predecible. Esto se debe a que la función se basa en un algoritmo matemático que utiliza la semilla proporcionada para generar números. Por lo tanto, si se utiliza la misma semilla, se obtendrá siempre la misma secuencia de números.

Para evitar esto, se suele utilizar la función `srand()` para establecer una semilla diferente cada vez que se ejecuta el programa, por ejemplo, utilizando el tiempo actual como se mostró en el ejemplo anterior.

**Otras funciones útiles:**

- `srand()` es necesario para generar una nueva secuencia de números aleatorios, pero si se desea generar números en un rango más específico, se puede utilizar la función `rand()` en combinación con operadores matemáticos, como se mostró en el ejemplo anterior.
- También está disponible la función `rand_r()` que permite especificar una semilla directamente en lugar de utilizar `srand()`. Esta función puede ser útil en programas que requieran concurrencia.

## Ver también

- [Documentación de la función rand() en C](https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm)
- [Generación de números aleatorios en otros lenguajes de programación](https://www.geeksforgeeks.org/generating-random-number-range-c/)
- [Ejemplo de juego en C utilizando números aleatorios](https://www.youtube.com/watch?v=5EosYa4gHBk)