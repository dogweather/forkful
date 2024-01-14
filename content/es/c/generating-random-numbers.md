---
title:                "C: Generando números aleatorios"
programming_language: "C"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué
Si eres un programador, es muy probable que hayas trabajado con números aleatorios en tus aplicaciones. Generar números aleatorios es una herramienta poderosa para crear aplicaciones emocionantes y dinámicas. Ya sea para juegos, sorteos o incluso para pruebas de estrés, la generación de números aleatorios puede ser una habilidad muy útil para cualquier programador.

## Cómo hacerlo
Para generar números aleatorios en C, podemos usar la función `rand()` de la biblioteca estándar `stdlib.h`. Esta función devuelve un número entero aleatorio entre 0 y `RAND_MAX`, que es una constante predefinida en la biblioteca. A continuación se muestra un ejemplo de cómo usar la función `rand()` en un programa simple de C:

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    // Generar un número aleatorio entre 1 y 10
    int numAleatorio = 1 + rand() % 10;
    printf("¡El número aleatorio es: %d!", numAleatorio);
    return 0;
}
```
El código anterior primero incluye las bibliotecas necesarias y luego usa la función `rand()` junto con el operador módulo para generar un número entre 0 y 9, y luego le sumamos 1 para obtener un número entre 1 y 10. Luego imprimimos el número aleatorio en la pantalla.

La función `rand()` también puede ser utilizada para generar números en otros rangos o incluso números con punto decimal. Para obtener más información sobre la función `rand()`, consulte la documentación en línea de C.

## Profundizando
La función `rand()` en realidad no genera números verdaderamente aleatorios. En lugar de eso, utiliza un algoritmo para producir una secuencia de números pseudoaleatoria. Esto significa que, aunque los números parecen aleatorios, en realidad son predecibles si conoces el algoritmo utilizado. Sin embargo, para la mayoría de las aplicaciones, esta pseudoaleatoriedad es suficiente.

También es importante tener en cuenta que, cada vez que se ejecute un programa que use la función `rand()`, la misma secuencia de números será generada. Para evitar esto, podemos usar la función `srand()` para inicializar una semilla aleatoria antes de usar la función `rand()`. De esta manera, la secuencia de números será diferente cada vez que ejecutemos el programa.

## Ver también
- [Documentación de la función `rand()` en C](https://www.cplusplus.com/reference/cstdlib/rand/)
- [Tutorial de programación en C](https://www.programiz.com/c-programming)