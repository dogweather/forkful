---
title:                "Organizando código en funciones"
date:                  2024-01-26T01:09:39.145568-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizando código en funciones"
programming_language: "C"
category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Organizar el código en funciones consiste en descomponer el código en bloques reutilizables que realizan tareas específicas. Esto hace que el código sea más fácil de leer, depurar y mantener.

## Cómo hacerlo:
Tomemos un ejemplo sencillo: digamos que quieres sumar dos números varias veces.

Sin funciones:
```C
#include <stdio.h>

int main() {
    int sum1 = 5 + 3;
    printf("Suma1: %d\n", sum1);
    
    int sum2 = 2 + 8;
    printf("Suma2: %d\n", sum2);
    
    // Más adiciones aquí...
    
    return 0;
}
```

Con funciones:
```C
#include <stdio.h>

int agregar(int a, int b) {
    return a + b;
}

int main() {
    int sum1 = agregar(5, 3);
    printf("Suma1: %d\n", sum1);
    
    int sum2 = agregar(2, 8);
    printf("Suma2: %d\n", sum2);
    
    // Utilizar la función agregar() para más adiciones...
    
    return 0;
}
```

Salida:
```
Suma1: 8
Suma2: 10
```

## Profundización
Antes de que C tuviera funciones, la programación se realizaba a menudo de manera lineal, al igual que una receta. Pero a medida que los programas crecían, la duplicación de código se convertía en un problema. Las funciones fueron la solución: nos permitieron ejecutar el mismo bloque de código desde diferentes partes de un programa sin tener que reescribirlo cada vez. Esto no solo ahorra espacio sino también tiempo al hacer actualizaciones: cambia la función en un lugar y cada parte de tu código que la utilice se actualiza.

Las alternativas a las funciones podrían incluir código en línea, macros o programación por copiar y pegar, pero estos pueden conducir a un código inflado, propenso a errores y difícil de mantener. Por el contrario, las funciones encapsulan funcionalidad, definen interfaces claras y pueden reducir efectos secundarios con el uso adecuado del ámbito.

Cuando implementas funciones, considera un par de detalles: uno, intenta hacer que hagan solo una cosa; esto se conoce como el Principio de Responsabilidad Única. Dos, los nombres son importantes; elige nombres descriptivos para las funciones y sus parámetros para hacer que tu código se documente a sí mismo.

## Véase también
Para más información sobre funciones en C, echa un vistazo a estos:

- Referencia de la Biblioteca Estándar de C: https://en.cppreference.com/w/c/header
- Programación en C: Un Enfoque Moderno por K.N. King: Un libro con una inmersión profunda en funciones.
- Learn-C.org: Sección de funciones: https://www.learn-c.org/es/Functions