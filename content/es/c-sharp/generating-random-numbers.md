---
title:                "Generando números aleatorios"
date:                  2024-01-20T17:48:54.564033-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generando números aleatorios"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qué es y Por Qué?

Generar números aleatorios es una forma de producir valores numéricos que no siguen un patrón predecible. Los programadores lo hacen para tareas como simulaciones, juegos y pruebas de seguridad; es esencial para añadir imprevisibilidad y diversidad a nuestros códigos.

## Cómo hacerlo:

Aquí tienes un ejemplo básico en C#:

```C#
using System;

namespace RandomNumberExample
{
    class Program
    {
        static void Main(string[] args)
        {
            Random random = new Random();
            int randomNumber = random.Next(0, 100); // Genera un número aleatorio entre 0 y 99
            Console.WriteLine($"Número aleatorio: {randomNumber}");
        }
    }
}
```

Salida posible:

```plaintext
Número aleatorio: 37
```

## Análisis Profundo:

Las primeras computadoras no tenían manera de generar números aleatorios. Usaban secuencias "pseudoaleatorias" que parecían aleatorias pero eran predecibles. Hoy en día, `System.Random` es estándar en C#, pero su algoritmo basado en semilla produce una secuencia predecible si se reutiliza la misma semilla.

Alternativas:

- `RNGCryptoServiceProvider`: Usa algoritmos más sofisticados para seguridad criptográfica.
- `RandomNumberGenerator.Create()`: Método de la clase abstracta `RandomNumberGenerator` para generar números más seguros.

Detalles de implementación:

- `Random.Next()`: Sobrecargado para que puedas definir un rango.
- Usa siempre una nueva instancia de `Random` para evitar repetir la secuencia si trabajas con hilos o generas números muy rápidamente.

## Consulta También:

- [`Random` class documentation](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-7.0)
- [MSDN - RNGCryptoServiceProvider](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=net-7.0)
- [Microsoft's documentation on RandomNumberGenerator](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.randomnumbergenerator?view=net-7.0)
