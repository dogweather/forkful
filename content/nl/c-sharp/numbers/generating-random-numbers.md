---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:34.702079-07:00
description: "Het genereren van willekeurige getallen in C# omvat de creatie van onvoorspelbare\
  \ numerieke waarden binnen een gespecificeerd bereik. Programmeurs\u2026"
lastmod: '2024-02-25T18:49:48.145515-07:00'
model: gpt-4-0125-preview
summary: "Het genereren van willekeurige getallen in C# omvat de creatie van onvoorspelbare\
  \ numerieke waarden binnen een gespecificeerd bereik. Programmeurs\u2026"
title: Willekeurige getallen genereren
---

{{< edit_this_page >}}

## Wat & Waarom?

Het genereren van willekeurige getallen in C# omvat de creatie van onvoorspelbare numerieke waarden binnen een gespecificeerd bereik. Programmeurs gebruiken deze methoden voor de implementatie van functies zoals cryptografie, simulaties en spellen waar onvoorspelbaarheid of de simulatie van realistische willekeur vereist is.

## Hoe:

De meest voorkomende manier om willekeurige getallen te genereren in C# is het gebruik van de `System.Random` klasse. Hier is een eenvoudig voorbeeld dat het gebruik demonstreert:

```C#
using System;

public class RandomNumberExample
{
    static void Main(string[] args)
    {
        Random random = new Random();
        int randomNumber = random.Next(1, 100); // Genereert een getal tussen 1 en 99
        Console.WriteLine($"Willekeurig getal: {randomNumber}");
    }
}
```

Dit zal een willekeurig getal uitvoeren zoals:

```
Willekeurig getal: 42
```

Voor het genereren van een willekeurig zwevendekommagetal tussen 0,0 en 1,0, kun je de `NextDouble` methode gebruiken:

```C#
double randomDouble = random.NextDouble();
Console.WriteLine($"Willekeurige dubbele: {randomDouble}");
```

Als je werkt aan een veiligheidsgevoelige applicatie die cryptografische willekeur vereist, is het beter om de `RNGCryptoServiceProvider` klasse te gebruiken die gevonden kan worden in `System.Security.Cryptography`:

```C#
using System;
using System.Security.Cryptography;

public class SecureRandomExample
{
    static void Main()
    {
        byte[] randomNumber = new byte[4]; // Creëert een 4-byte lang willekeurig getal
        using (RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider())
        {
            rng.GetBytes(randomNumber);
        }
        int value = BitConverter.ToInt32(randomNumber, 0);
        Console.WriteLine($"Cryptografisch veilig willekeurig getal: {value}");
    }
}
```

## Diepgaande duik

Het genereren van willekeurige getallen in C# is in de loop der jaren geëvolueerd. Aanvankelijk was de `System.Random` klasse de gangbare methode voor het genereren van pseudo-willekeurige getallen. Het is pseudo-willekeurig omdat, gegeven een specifieke seedwaarde, het dezelfde reeks getallen zal produceren, wat nuttig kan zijn voor debugging of herhaalbaarheid van tests.

Hoewel voldoende voor basisbehoeften, is `System.Random` niet thread-safe en kan voorspelbare uitkomsten produceren, wat niet geschikt is voor op veiligheid-afhankelijke applicaties. Deze beperking leidde tot de introductie van de `RNGCryptoServiceProvider` voor cryptografische willekeur, die veiliger maar ook meer bronintensief is.

Een alternatief in .NET Core en .NET 5+ is de `RandomNumberGenerator` klasse in `System.Security.Cryptography` voor het veilig genereren van willekeurige getallen, bedoeld als een modernere en gebruiksvriendelijkere optie in vergelijking met `RNGCryptoServiceProvider`.

Elke methode voor het genereren van willekeurige getallen in C# heeft zijn plaats, afhankelijk van de vereisten van de applicatie. Voor de meeste toepassingen volstaat `System.Random`, maar voor diegenen die veilige, onvoorspelbare willekeurige getallen vereisen, bieden de cryptografische klassen een robuust alternatief.
