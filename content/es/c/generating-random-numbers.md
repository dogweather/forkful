---
title:    "C: Generando números aleatorios"
keywords: ["C"]
---

{{< edit_this_page >}}

# Por qué

Generar números aleatorios es un concepto importante en la programación ya que permite crear programas más dinámicos e interactivos. Con los números aleatorios, se pueden simular escenarios diferentes cada vez que se ejecuta un programa, lo que lo hace más interesante para los usuarios.

# Cómo hacerlo

En C, hay varias formas de generar números aleatorios. Una de ellas es usando la función `rand()` de la biblioteca estándar `stdlib.h`. Esta función devuelve un número entero aleatorio entre 0 y `RAND_MAX`, que es una constante definida en la misma biblioteca.

Para usar esta función, primero debemos incluir la biblioteca `stdlib.h` en nuestro programa. Luego, podemos llamar a la función `rand()` para obtener un número aleatorio. Por ejemplo:

```C
#include <stdlib.h>
#include <stdio.h>

int main() {
  int num = rand();
  printf("Número aleatorio generado: % d \n", num);
  return 0;
}
```
La salida de este programa será un número aleatorio generado cada vez que se ejecuta. Puedes probarlo tú mismo ejecutando el programa varias veces.

# Inmersión profunda

Sin embargo, hay una cosa importante que debemos tener en cuenta al generar números aleatorios en C. La función `rand()` genera los números aleatorios utilizando un algoritmo predeterminado, y este algoritmo se repite cada vez que se ejecuta un programa. Esto significa que, aunque se obtienen números diferentes cada vez, estos números seguirán una secuencia predecible.

Para evitar esto, necesitamos inicializar la "semilla" del generador de números aleatorios antes de llamar a la función `rand()`. Esta semilla establece un punto de partida para el algoritmo de generación de números aleatorios, lo que garantiza que se generen números diferentes cada vez que se ejecuta el programa. La forma de inicializar la semilla es mediante la función `srand()` de la misma biblioteca, que toma como argumento un número entero que actuará como semilla.

Una forma común de obtener una semilla aleatoria es usando la función `time()` de la biblioteca `time.h`, que devuelve la hora actual en segundos. Luego, podemos pasar este valor a la función `srand()` para obtener diferentes números aleatorios cada vez que se ejecuta el programa. Por ejemplo:

```C
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

int main() {
  int num;
  srand(time(NULL)); //inicilizar la semilla con la hora actual
  num = rand();
  printf("Número aleatorio generado: % d \n", num);
  return 0;
}
```

# Ver también

Para más información sobre cómo generar números aleatorios en C, puedes consultar estos recursos:

- [Documentación oficial de la función `rand()` en la biblioteca `stdlib.h`](https://en.cppreference.com/w/c/numeric/random/rand)
- [Tutorial sobre cómo generar números aleatorios en C](https://www.tutorialspoint.com/c_standard_library/c_function_srand.htm)
- [Explicación más detallada sobre cómo funciona la función `rand()`](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)