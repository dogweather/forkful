---
title:    "C#: Generando números aleatorios"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por qué

Generar números aleatorios es una habilidad importante en la programación, ya que puede ser utilizado en múltiples aplicaciones como juegos, simulaciones o pruebas. También es útil para generar datos de prueba y explorar algoritmos.

## Cómo hacerlo

Para generar números aleatorios en C#, utilizamos la clase `Random` y su método `Next()`. Veamos un ejemplo en código:

```C#
// Creamos una instancia de la clase Random
Random rnd = new Random();

// Generamos un número aleatorio entre 0 y 100
int num = rnd.Next(0, 101);

// Imprimimos el resultado 
Console.WriteLine(num);
```

Este código creará una instancia de la clase `Random` y generará un número aleatorio entre 0 y 100, el cual será almacenado en la variable `num`. Luego, lo imprimimos por consola y podemos ver el resultado.

También es posible generar números aleatorios de otros tipos de datos, como números decimales o caracteres. Solo debemos especificar el rango adecuado en el método `Next()` y convertir el resultado al tipo de dato deseado.

## Profundizando

Detrás de la generación de números aleatorios en C#, se utiliza un algoritmo llamado "Generador lineal congruente". Este algoritmo utiliza una fórmula matemática para producir secuencias de números pseudo-aleatorios, es decir, números que parecen aleatorios pero que en realidad están determinados por la semilla inicial que se le pasa al generador.

Es importante tener en cuenta que los números generados por este algoritmo no son realmente aleatorios, ya que pueden ser recreados si se conoce la semilla inicial. Sin embargo, para la mayoría de aplicaciones prácticas, este nivel de "aleatoriedad" es suficiente.

## Ver también

- Documentación de la clase `Random` en la Microsoft Docs: https://docs.microsoft.com/es-es/dotnet/api/system.random?view=netcore-3.1
- Otros algoritmos para generar números aleatorios en C#: https://www.dotnetperls.com/random