---
title:                "Java: Generando números aleatorios"
programming_language: "Java"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué generar números aleatorios en Java

Generar números aleatorios en Java puede ser útil para una variedad de aplicaciones, como juegos, simulaciones o cifrado. También puede ser útil para generar datos de prueba para probar el rendimiento de un programa.

## Cómo generar números aleatorios en Java

Para generar números aleatorios en Java, podemos utilizar la clase Random de la librería estándar. Primero, debemos importar la clase en nuestro código:

```Java 
import java.util.Random;
```

Luego, podemos crear una instancia de la clase y utilizar el método nextInt() para generar un número entero aleatorio dentro de un rango específico:

```Java
Random random = new Random();
int numeroAleatorio = random.nextInt(100); // genera un número entre 0 y 99
```

También podemos utilizar otros métodos como nextDouble() para generar un número decimal aleatorio o nextBoolean() para generar un valor booleano aleatorio. Además, podemos especificar un valor de semilla en el constructor de la clase si queremos generar la misma secuencia de números aleatorios en cada ejecución del programa.

## Profundizando en la generación de números aleatorios

La clase Random utiliza un algoritmo llamado generador de números pseudoaleatorios (PRNG), que utiliza una semilla para producir una secuencia aparentemente aleatoria de números. Es importante tener en cuenta que estos números no son verdaderamente aleatorios, ya que pueden repetirse en diferentes ejecuciones. Sin embargo, para la mayoría de las aplicaciones, esto es suficiente.

Si necesitamos generar números realmente aleatorios, podemos utilizar la clase SecureRandom, que utiliza una fuente externa de entropía para producir números aleatorios más seguros. También podemos utilizar otras librerías de terceros que implementan diferentes algoritmos de generación de números aleatorios, como Mersenne Twister.

## Vea también

- Documentación oficial de la clase Random en Java: https://docs.oracle.com/javase/8/docs/api/java/util/Random.html
- Ejemplos de uso de la clase Random en Java: https://www.baeldung.com/java-generate-random-long-float-integer-double-string
- Más información sobre generadores de números pseudoaleatorios: https://en.wikipedia.org/wiki/Pseudorandom_number_generator