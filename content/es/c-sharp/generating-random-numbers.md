---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Generando números aleatorios en C#

**Contenido**
- ¿Qué y por qué?
- ¿Cómo se hace?
- Más allá
- Consulta también

## ¿Qué y por qué?

Generar números aleatorios es la acción de producir números que no siguen ningún patrón predecible. Los programadores generan números aleatorios para diversos fines como pruebas automatizadas, juegos y simulaciones.

## ¿Cómo se hace?

Echale un vistazo a estos ejemplos de código para entender mejor cómo generar números aleatorios en C#.

```C#
// Importa la clase Random
using System;

class Program
{
    static void Main()
    {
        // Crear una instancia de la clase Random
        Random random = new Random();

        // Generar un número aleatorio entre 0 y 10
        int aleatorio = random.Next(0, 11);
        Console.WriteLine("Número aleatorio: " + aleatorio);
    }
}
```
Este código producirá una salida como la que sigue. El número variará cada vez que ejecute el programa.

```
Número aleatorio: 7
```

## Más allá

**Contexto histórico:** La generación de números aleatorios se remonta a los primeros ordenadores. Sin embargo, con la evolución del software y hardware, la generación de números aleatorios ahora se puede lograr con programas como C#.

**Alternativas:** Además de la clase `Random`, otras alternativas para generar números aleatorios en C# incluyen `RNGCryptoServiceProvider` para fines de seguridad y `System.Security.Cryptography.RandomNumberGenerator` para generar un array de bytes aleatorios.

**Detalles de implementación:** La clase `Random` en C# usa una semilla basada en el reloj del sistema para generar un número aleatorio. Dado que la semilla es el tiempo actual, dos instancias de `Random` creadas al mismo tiempo pueden generar la misma serie de números.

## Consulta también

1. Clase Random en C# [(Microsoft Docs)](https://docs.microsoft.com/es-es/dotnet/api/system.random?view=net-5.0)
3. RNGCryptoServiceProvider [(Microsoft Docs)](https://docs.microsoft.com/es-es/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=net-5.0)
4. System.Security.Cryptography.RandomNumberGenerator [(Microsoft Docs)](https://docs.microsoft.com/es-es/dotnet/api/system.security.cryptography.randomnumbergenerator?view=net-5.0)