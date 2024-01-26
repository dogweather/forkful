---
title:                "Generando números aleatorios"
date:                  2024-01-20T17:49:28.246694-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generando números aleatorios"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generar números aleatorios es como sacar una carta al azar de un mazo; jamás sabes qué vas a obtener. En programación, esto es clave para juegos, pruebas, seguridad y simulaciones.

## How to:
Java nos facilita la creación de números aleatorios. Aquí te muestro cómo.

```java
import java.util.Random;

public class GeneradorAleatorio {
    public static void main(String[] args) {
        Random random = new Random();

        int numeroEnteroAleatorio = random.nextInt(100); // Entre 0 y 99
        double numeroRealAleatorio = random.nextDouble(); // Entre 0.0 y 1.0

        System.out.println("Número entero aleatorio: " + numeroEnteroAleatorio);
        System.out.println("Número real aleatorio: " + numeroRealAleatorio);
    }
}
```

Ejecutar el código te lanzará dos números, uno entero y otro real, así:

```
Número entero aleatorio: 42
Número real aleatorio: 0.73046875
```

## Deep Dive:
La generación de números aleatorios en Java ha evolucionado. Antes usábamos `Math.random()` que todavía funciona, pero `Random` es más flexible. ¿Sabías que los números "aleatorios" en la computación no son realmente aleatorios? Se basan en algoritmos predecibles, por eso se les llama pseudoaleatorios.

Para necesidades de alta seguridad, como criptografía, no uses `Random`. En su lugar, `SecureRandom` es la opción, ya que produce números menos predecibles.

Antes de `Random`, los programadores recurrían a rutinas matemáticas para crear su propia suerte, pero la estandarización de las bibliotecas facilitó enormemente las cosas.

## See Also:
Aquí tienes algunas fuentes para expandir tu conocimiento:

- [Java Random Class Doc](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/Random.html)
- [SecureRandom Class Doc](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)
- [Math.random() vs new Random.nextInt(int)](https://stackoverflow.com/questions/738629/math-random-versus-random-nextintint)
