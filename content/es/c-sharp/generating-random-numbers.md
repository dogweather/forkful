---
title:    "C#: Generando números aleatorios"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

##¿Por qué Generar Números Aleatorios?

La generación de números aleatorios es una práctica común en la programación. Puede ser útil en muchos escenarios, como en juegos, encriptación y simulaciones. También puede ser una forma divertida de jugar con los algoritmos de programación y mejorar tus habilidades.

## Cómo Generar Números Aleatorios en C #

Generar números aleatorios en C # es sencillo y se puede hacer utilizando la clase `Random`. Aquí hay un ejemplo de cómo generar un número entero aleatorio entre 1 y 10:

```C#
//Declara una instancia de la clase Random
Random rand = new Random();
//Genera un número aleatorio entre 1 y 10
int numAleatorio = rand.Next(1, 11);
```

También puedes usar `NextDouble ()` para generar un número decimal aleatorio entre 0 y 1.

```C#
Random rand = new Random();
//Genera un número decimal aleatorio entre 0 y 1
double numAleatorio = rand.NextDouble();
```

## Profundizando en la Generación de Números Aleatorios

La clase `Random` utiliza un algoritmo para generar números aleatorios que se basa en una semilla. La semilla es un número entero que se utiliza como punto de partida para el algoritmo de generación de números aleatorios. Si no se especifica una semilla, se toma la hora actual como semilla. Esto significa que cada vez que ejecutes el programa, obtendrás una secuencia diferente de números aleatorios.

Si deseas obtener la misma secuencia de números aleatorios en cada ejecución del programa, puedes especificar una semilla fija en el constructor de la clase `Random`.

```C#
//Declara una instancia de la clase Random con una semilla fija
Random rand = new Random(42); //42 es solo un ejemplo de una semilla
```

La generación de números aleatorios en computadoras es en realidad un proceso pseudoaleatorio. Esto significa que, aunque los números pueden parecer aleatorios, en realidad están siguiendo un algoritmo matemático. Ten esto en cuenta si necesitas generar números verdaderamente aleatorios para propósitos de seguridad.

## Ver También

- [Documentación oficial de Microsoft para la clase Random en C#](https://docs.microsoft.com/es-es/dotnet/api/system.random)
- [Más ejemplos de generación de números aleatorios en C#](https://www.c-sharpcorner.com/blogs/random-number-generation-in-c-sharp1)
- [Introducción a los Generators en C# 9](https://docs.microsoft.com/es-es/dotnet/csharp/whats-new/csharp-9#generators)