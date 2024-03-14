---
date: 2024-01-27 20:33:01.237178-07:00
description: "\xC5 generere tilfeldige tall i C# involverer opprettelsen av uforutsigbare\
  \ numeriske verdier innenfor et spesifisert omr\xE5de. Programmerere bruker disse\u2026"
lastmod: '2024-03-13T22:44:40.791161-06:00'
model: gpt-4-0125-preview
summary: "\xC5 generere tilfeldige tall i C# involverer opprettelsen av uforutsigbare\
  \ numeriske verdier innenfor et spesifisert omr\xE5de. Programmerere bruker disse\u2026"
title: Generering av tilfeldige tall
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å generere tilfeldige tall i C# involverer opprettelsen av uforutsigbare numeriske verdier innenfor et spesifisert område. Programmerere bruker disse metodene for å implementere funksjoner som kryptografi, simuleringer og spill hvor uforutsigbarhet eller simulering av ekte verdens tilfeldighet kreves.

## Hvordan:

Den vanligste måten å generere tilfeldige tall i C# er ved bruk av `System.Random`-klassen. Her er et enkelt eksempel som demonstrerer bruken:

```C#
using System;

public class RandomNumberExample
{
    static void Main(string[] args)
    {
        Random random = new Random();
        int randomNumber = random.Next(1, 100); // Genererer et tall mellom 1 og 99
        Console.WriteLine($"Tilfeldig tall: {randomNumber}");
    }
}
```

Dette vil gi ut et tilfeldig tall slik som:

```
Tilfeldig tall: 42
```

For å generere et tilfeldig flyttall mellom 0,0 og 1,0, kan du bruke `NextDouble`-metoden:

```C#
double randomDouble = random.NextDouble();
Console.WriteLine($"Tilfeldig dobbelt: {randomDouble}");
```

Hvis du jobber med en sikkerhetssensitiv applikasjon som krever kryptografisk tilfeldighet, er det bedre å bruke `RNGCryptoServiceProvider`-klassen som finnes i `System.Security.Cryptography`:

```C#
using System;
using System.Security.Cryptography;

public class SecureRandomExample
{
    static void Main()
    {
        byte[] randomNumber = new byte[4]; // Oppretter et 4-byte langt tilfeldig tall
        using (RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider())
        {
            rng.GetBytes(randomNumber);
        }
        int value = BitConverter.ToInt32(randomNumber, 0);
        Console.WriteLine($"Kryptografisk sikkert tilfeldig tall: {value}");
    }
}
```

## Dykk dypere

Generering av tilfeldige tall i C# har utviklet seg over årene. Opprinnelig var `System.Random`-klassen gå-til-metoden for å generere pseudo-tilfeldige tall. Det er pseudo-tilfeldig fordi, gitt en spesifikk seed-verdi, vil den produsere samme sekvens av tall, noe som kan være nyttig for feilsøking eller gjentakbarhet av tester.

Selv om den er tilstrekkelig for grunnleggende behov, er ikke `System.Random` trådsikker og kan produsere forutsigbare utfall, noe som ikke er egnet for sikkerhetsavhengige applikasjoner. Denne begrensningen førte til introduksjonen av `RNGCryptoServiceProvider` for kryptografisk tilfeldighet, som er mer sikker, men også mer ressurskrevende.

Et alternativ i .NET Core og .NET 5+ er `RandomNumberGenerator`-klassen i `System.Security.Cryptography` for å generere tilfeldige tall på en sikker måte, som er ment som et mer moderne og brukervennlig alternativ sammenlignet med `RNGCryptoServiceProvider`.

Hver metode for å generere tilfeldige tall i C# har sin plass avhengig av applikasjonens krav. For de fleste applikasjoner, er `System.Random` tilstrekkelig, men for de som krever sikre, uforutsigbare tilfeldige tall, gir de kryptografiske klassene et robust alternativ.
