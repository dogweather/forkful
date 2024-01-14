---
title:                "C#: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué generar números aleatorios es importante

Generar números aleatorios es una habilidad importante para cualquier programador de C#. Conviene saber cómo hacerlo, ya que permite crear programas más dinámicos y con resultados diferentes en cada ejecución. Además, puede ser útil en juegos de azar, sorteos y otras aplicaciones.

## Cómo generar números aleatorios en C#

La biblioteca .NET incluye la clase `Random` que facilita la generación de números aleatorios en C#. Aquí hay un ejemplo de código que muestra cómo generar cinco números aleatorios entre 1 y 10 y mostrarlos en la consola:

```C#
// Importar la clase Random
using System;

// Crear una instancia de la clase Random
Random rnd = new Random();

// Generar 5 números aleatorios entre 1 y 10
for (int i = 0; i < 5; i++)
{
    int num = rnd.Next(1, 11);
    Console.WriteLine(num);
}
```

El resultado de este código podría ser:

```
8
2
6
4
10
```

Puedes notar que cada vez que se ejecuta el programa, los números generados son diferentes.

## Profundizando en la generación de números aleatorios

La clase `Random` utiliza un algoritmo para generar números pseudoaleatorios a partir de una semilla inicial. Esto significa que los números aparentemente aleatorios no son realmente aleatorios, sino que siguen un patrón predecible. Siempre comienzan desde la misma semilla, los resultados serán los mismos. Sin embargo, en la práctica, estos números son suficientemente aleatorios para la mayoría de las aplicaciones.

También es importante tener en cuenta que la clase `Random` no es segura para usar en entornos de múltiples hilos, ya que puede causar conflictos de acceso a la misma instancia desde diferentes hilos. Para evitar este problema, puedes utilizar la clase `ThreadLocal<Random>` para crear una instancia individual para cada hilo.

## Ver también

- [Documentación de la clase Random en Microsoft Docs](https://docs.microsoft.com/es-es/dotnet/api/system.random)
- [Artículo sobre la seguridad de la clase Random en StackOverflow (en inglés)](https://stackoverflow.com/questions/767999/random-number-generator-only-generates-one-random-number)