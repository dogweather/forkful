---
title:                "Java: Generación de números aleatorios"
simple_title:         "Generación de números aleatorios"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Por qué generar números aleatorios en Java?

Generar números aleatorios es una práctica común en la programación para simular situaciones y tomar decisiones de manera impredecible. Además, también se utiliza en la criptografía y en la creación de juegos.

## Cómo hacerlo en Java

Para generar números aleatorios en Java, se utiliza la clase Random. Primero, se debe importar la clase en la parte superior del código:

```Java
import java.util.Random;
```

Luego, se instancia un objeto de la clase Random:

```Java
Random random = new Random();
```

Para generar un número aleatorio entero, se utiliza el método nextInt() de la clase Random. Por ejemplo, si queremos generar un número entre 1 y 10:

```Java
int randomNumber = random.nextInt(10) + 1;
System.out.println(randomNumber); // Output: 5 (el resultado será un número entre 1 y 10)
```

Para generar un número aleatorio de tipo double, se utiliza el método nextDouble(). Por ejemplo:

```Java
double randomDouble = random.nextDouble();
System.out.println(randomDouble); // Output: 0.653 (el resultado será un número entre 0 y 1)
```

También se pueden generar números aleatorios dentro de un rango específico utilizando el método nextInt() junto con la fórmula correspondiente. Por ejemplo, si queremos generar un número entre 50 y 100:

```Java
int randomNumberInRange = random.nextInt(51) + 50;
System.out.println(randomNumberInRange); // Output: 82 (el resultado será un número entre 50 y 100)
```

## Profundizando en la generación de números aleatorios

Java utiliza un algoritmo llamado "Linear Congruential Generator" para generar números aleatorios utilizando la clase Random. Este algoritmo utiliza una semilla (seed) para comenzar a generar una secuencia de pseudoaleatorios. Si no se especifica una semilla, la clase Random utilizará la hora del sistema como semilla por defecto.

Es importante tener en cuenta que, aunque los números generados parecen ser aleatorios, en realidad siguen una secuencia predecible basada en la semilla utilizada. Por lo tanto, siempre se recomienda utilizar una semilla diferente si se quiere obtener una secuencia diferente de números aleatorios.

## Ver también

- [Documentación de la clase Random en Java](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Tutorial sobre generación de números aleatorios en Java](https://www.geeksforgeeks.org/random-setseed-method-in-java-with-examples/)
- [Métodos matemáticos para generar números aleatorios](https://www.geeksforgeeks.org/maths-behind-simulating-rolling-of-two-dice/)