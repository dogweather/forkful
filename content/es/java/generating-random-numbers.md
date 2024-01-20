---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Generación de Números Aleatorios en Java

## ¿Qué y Por Qué?
Generar números aleatorios significa producir números que no tienen un patrón predecible. Los programadores lo hacen cuando necesitan una muestra aleatoria, por ejemplo, en juegos, en la simulación y en criptografía.

## ¿Cómo se hace? 
Generar números aleatorios en Java es sencillo gracias a la clase `Random` de Java:

```Java
import java.util.Random;

public class Main {
  public static void main(String[] args) {
    Random rand = new Random();
    int numeroAleatorio = rand.nextInt(100); // Genera un número aleatorio entre 0 y 99
    System.out.println("El número aleatorio es: " + numeroAleatorio);
  }
}
```
Ejecutar este programa imprimirá en la consola un número aleatorio como este:
```
El número aleatorio es: 42
```
## Profundiza
Históricamente, Java proporciona varias maneras de generar números aleatorios. La anterior es la más común, pero existen otras como `Math.random()` y `ThreadLocalRandom.current()`.

Pueden ser alternativas útiles según tu caso. Por ejemplo, `Math.random()` es ideal si sólo necesitas un número de doble precisión entre 0.0 y 1.0.

En cuanto a los detalles de implementación, el método `nextInt(100)` genera un número aleatorio entre 0 (incluido) y 100 (excluido). Usarlo sin argumentos da un entero aleatorio en todo el rango de valores de int (ambos extremos incluidos).

```Java
int noRango = rand.nextInt(); // Genera un número aleatorio en todo el rango de valores de int
```

## Ver También
1. Para aprender más sobre la clase `Random`, puedes visitar su documentación en la [Java API](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html).
2. Para aprender sobre `Math.random()`, puedes consultar su documentación en la [Java API](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#random--).
3. Información sobre `ThreadLocalRandom` en su [página en la Java API](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ThreadLocalRandom.html) puede ser útil para programas multihilo.