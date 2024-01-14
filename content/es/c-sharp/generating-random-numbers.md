---
title:                "C#: Generando números aleatorios"
programming_language: "C#"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

### Por qué generar números aleatorios en programación

La generación de números aleatorios es una técnica común en programación para crear resultados aleatorios y evitar patrones predecibles en la ejecución de un programa. Puede ser útil en una variedad de aplicaciones, como juegos, simulaciones y criptografía.

### Cómo hacerlo en C#

En C#, podemos generar números aleatorios utilizando la clase `Random` y su método `Next()`. Veamos un ejemplo básico:

```C#
// Declarar un objeto de tipo Random
Random rnd = new Random();

// Generar un número aleatorio entre 1 y 10
int numeroAleatorio = rnd.Next(1, 11);

// Imprimir el resultado
Console.WriteLine("El número aleatorio es: " + numeroAleatorio);
```

El código anterior creará un objeto `Random` y llamará al método `Next()` indicando el rango en el que se desea generar el número aleatorio. El resultado será un número entero aleatorio entre 1 y 10.

También es posible generar números aleatorios de otros tipos de datos, como por ejemplo, números decimales o caracteres. Es importante mencionar que la clase `Random` utiliza un algoritmo para generar estos números, por lo que no son completamente aleatorios, pero son suficientemente impredecibles para muchos casos de uso.

### Profundizando en la generación de números aleatorios

Para aquellos interesados en conocer cómo funcionan los algoritmos de generación de números aleatorios, existen técnicas más avanzadas que utilizan otros métodos y ecuaciones matemáticas para crear resultados más precisos y aleatorios.

Entre estas técnicas se encuentra la generación de números pseudoaleatorios, que son secuencias de números que parecen ser aleatorios, pero en realidad siguen un patrón determinado. Estas secuencias pueden ser útiles en ciertos contextos, como por ejemplo, en simulaciones científicas.

También existen algoritmos de generación de números criptográficos, que son utilizados en el ámbito de la seguridad informática para garantizar la máxima aleatoriedad y proteger datos sensibles.

En resumen, la generación de números aleatorios es una herramienta útil y versátil en programación, con diversas aplicaciones y técnicas para diferentes propósitos.

### Ver también

- [Documentación oficial de Microsoft sobre la clase `Random` en C#](https://docs.microsoft.com/es-es/dotnet/api/system.random?view=netcore-3.1)
- [Article on random number generation algorithms](https://www.geeksforgeeks.org/pseudo-random-number-generator-prng/)
- [Overview of cryptographically secure pseudo-random number generators](https://www.sciencedirect.com/science/article/abs/pii/S0898122107000947)