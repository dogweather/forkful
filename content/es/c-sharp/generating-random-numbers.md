---
title:                "Generación de números aleatorios"
date:                  2024-01-27T20:32:38.198865-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generación de números aleatorios"

category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Generar números aleatorios en C# implica la creación de valores numéricos impredecibles dentro de un rango especificado. Los programadores utilizan estos métodos para implementar características como la criptografía, simulaciones y juegos donde se requiere la imprevisibilidad o la simulación de aleatoriedad del mundo real.

## Cómo hacerlo:

La manera más común de generar números aleatorios en C# es usando la clase `System.Random`. Aquí hay un ejemplo simple que demuestra su uso:

```C#
using System;

public class EjemploNumeroAleatorio
{
    static void Main(string[] args)
    {
        Random random = new Random();
        int numeroAleatorio = random.Next(1, 100); // Genera un número entre 1 y 99
        Console.WriteLine($"Número aleatorio: {numeroAleatorio}");
    }
}
```

Esto producirá un número aleatorio como:

```
Número aleatorio: 42
```

Para generar un número de punto flotante aleatorio entre 0.0 y 1.0, puedes usar el método `NextDouble`:

```C#
double dobleAleatorio = random.NextDouble();
Console.WriteLine($"Doble aleatorio: {dobleAleatorio}");
```

Si estás trabajando en una aplicación sensible a la seguridad que requiere aleatoriedad criptográfica, es mejor usar la clase `RNGCryptoServiceProvider` encontrada en `System.Security.Cryptography`:

```C#
using System;
using System.Security.Cryptography;

public class EjemploAleatorioSeguro
{
    static void Main()
    {
        byte[] numeroAleatorio = new byte[4]; // Crea un número aleatorio de 4 bytes de largo
        using (RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider())
        {
            rng.GetBytes(numeroAleatorio);
        }
        int valor = BitConverter.ToInt32(numeroAleatorio, 0);
        Console.WriteLine($"Número aleatorio criptográficamente seguro: {valor}");
    }
}
```

## Análisis Profundo

La generación de números aleatorios en C# ha evolucionado a lo largo de los años. Inicialmente, la clase `System.Random` era la opción predilecta para generar números pseudoaleatorios. Es pseudoaleatorio porque, dado un valor semilla específico, producirá la misma secuencia de números, lo que puede ser útil para depuración o repetibilidad de pruebas.

Aunque es suficiente para necesidades básicas, `System.Random` no es seguro para hilos y puede producir resultados predecibles, lo cual no es adecuado para aplicaciones dependientes de la seguridad. Esta limitación llevó a la introducción del `RNGCryptoServiceProvider` para la aleatoriedad criptográfica, que es más seguro pero también más intensivo en recursos.

Una alternativa en .NET Core y .NET 5+ es la clase `RandomNumberGenerator` en `System.Security.Cryptography` para generar números aleatorios de manera segura, que está pensada como una opción más moderna y fácil de usar en comparación con `RNGCryptoServiceProvider`.

Cada método de generación de números aleatorios en C# tiene su lugar dependiendo de los requisitos de la aplicación. Para la mayoría de las aplicaciones, `System.Random` es suficiente, pero para aquellas que requieren números aleatorios seguros e impredecibles, las clases criptográficas proporcionan una alternativa robusta.
