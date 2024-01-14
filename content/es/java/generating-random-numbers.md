---
title:    "Java: Generando números aleatorios"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por qué generar números aleatorios en Java

Generar números aleatorios es una herramienta útil en la programación de Java porque permite a los desarrolladores crear programas más dinámicos y variados. Al incorporar números aleatorios, se pueden crear juegos, sorteos, algoritmos de inteligencia artificial y muchas otras aplicaciones interesantes.

## Cómo generar números aleatorios en Java

La clase 'Random' en Java es una de las formas más comunes de generar números aleatorios. Primero, importa la clase en tu programa:

```Java
import java.util.Random;
```

Luego, se puede crear una instancia de la clase y utilizar sus métodos para generar números aleatorios:

```Java
Random numeroAleatorio = new Random();

// Generar un número entero aleatorio entre 0 y 100
int numero = numeroAleatorio.nextInt(101); 

// Generar un número real entre 0 y 1
double numeroReal = numeroAleatorio.nextDouble(); 

// Generar un número entero aleatorio entre un rango específico
int numeroRango = numeroAleatorio.nextInt(10) + 5; // Genera un número entre 5 y 14
```

Puedes ver que al usar el método `nextInt()`, puedes especificar un límite para el número aleatorio. También puedes utilizar otros métodos, como `nextBoolean()` y `nextFloat()`, para generar diferentes tipos de datos aleatorios.

## Profundizando sobre la generación de números aleatorios

Aunque la clase `Random` es una forma conveniente de generar números aleatorios, no es verdaderamente aleatoria. En realidad, se basa en un algoritmo que calcula una secuencia de números que parecen aleatorios. Esto significa que, si se utiliza sin ajustar correctamente, se pueden generar patrones predecibles.

Una forma de abordar este problema es proporcionar una semilla al constructor de la clase `Random` para que genere una secuencia de números diferente cada vez que se ejecute el programa. De esta forma, los números resultantes serán más imprevisibles.

Otra opción es utilizar la clase `SecureRandom`, que utiliza un algoritmo de generación de números realmente aleatorios, aunque un poco más lento que la clase `Random`.

## Ver también

- [Clase Random en la documentación de Java](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Clase SecureRandom en la documentación de Java](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)