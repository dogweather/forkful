---
title:                "Generando números aleatorios"
html_title:           "Java: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué

Generar números aleatorios en Java puede ser útil para una variedad de aplicaciones, como juegos, simulaciones y pruebas de algoritmos. También se puede utilizar para generar datos de prueba para probar el funcionamiento de una aplicación.

## Cómo hacerlo

Generar números aleatorios en Java es muy sencillo utilizando la clase `Random`. Para ello, primero debemos importar la clase en nuestro código:

```Java
import java.util.Random;
```

Luego, podemos crear una instancia de la clase `Random` y utilizar sus métodos para generar números aleatorios:

```Java
// Crear una instancia de la clase Random
Random random = new Random();

// Generar un número entero aleatorio entre 0 y 100 (excluyendo 100)
int randomNumber = random.nextInt(100);

// Generar un número real aleatorio entre 0 y 1
double randomDouble = random.nextDouble();
```

Podemos especificar un rango diferente pasando los valores mínimo y máximo como parámetros a los métodos `nextInt()` y `nextDouble()`. También podemos generar números aleatorios de otros tipos de datos, como `long` y `float`.

Además, podemos utilizar métodos como `nextInt(n)` para generar un número entero aleatorio entre 0 (incluyendo 0) y el número especificado `n` (excluyendo `n`). Esto puede ser útil para generar índices aleatorios para acceder a elementos de una lista o array.

Podemos ver el resultado de nuestro código con un simple `System.out.println()`:

```Java
System.out.println(randomNumber); // Imprime un número entero aleatorio entre 0 y 100
System.out.println(randomDouble); // Imprime un número real aleatorio entre 0 y 1
```

## Profundizando

La clase `Random` utiliza un algoritmo para generar números aleatorios. Este algoritmo se basa en una semilla (un número inicial) y utiliza operaciones matemáticas para generar una secuencia de números aparentemente aleatoria.

Es importante tener en cuenta que los números generados por la clase `Random` no son completamente aleatorios, sino que siguen un patrón predecible. Es por eso que, si creamos una instancia de la clase `Random` con la misma semilla, obtendremos la misma secuencia de números aleatorios.

Para generar números más cercanos a la verdadera aleatoriedad, Java ofrece la clase `SecureRandom`, que utiliza una fuente de entropía externa como semilla para su algoritmo. Sin embargo, esta clase puede ser menos eficiente que la clase `Random` ya que requiere acceder a recursos externos.

## Ver también

- Documentación oficial de la clase `Random` en Java: https://docs.oracle.com/javase/8/docs/api/java/util/Random.html
- Tutorial sobre cómo generar números aleatorios en Java: https://www.geeksforgeeks.org/generating-random-numbers-in-java/