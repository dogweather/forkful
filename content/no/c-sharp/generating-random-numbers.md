---
title:                "Generering av tilfeldige tall"
date:                  2024-01-20T17:48:44.191510-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å generere tilfeldige tall er å lage nummer som ikke har noe forutsigbart mønster. Programmere bruker dette for alt fra spill til sikkerhet, hvor upålitelighet er viktig.

## Hvordan:
La oss kaste oss uti. For å generere et tilfeldig tall:

```C#
using System;

public class RandomNumberGenerator
{
    static void Main(string[] args)
    {
        Random random = new Random();
        int randomNumber = random.Next(1, 100); // fra 1 til 99
        Console.WriteLine(randomNumber);
    }
}
```

Kjører du dette, får du et nytt tall mellom 1 og 99 hver gang.

Det blir mer interessant hvis vi ønsker et desimaltall:

```C#
double randomDouble = random.NextDouble(); // fra 0.0 til 1.0
Console.WriteLine(randomDouble);
```

Eller kanskje et tilfeldig tall i et større intervall? Slik gjør du det:

```C#
int randomRange = random.Next(1000, 10000); // fra 1000 til 9999
Console.WriteLine(randomRange);
```

## Dypdykk:
Historisk sett har Ekte Tilfeldighet vært vanskelig å få til i programmering. Hva vi ofte kaller "tilfeldig" er egentlig "pseudotilfeldig", generert gjennom forutsigbare algoritmer. Men for de fleste bruksområder er pseudotilfeldighet godt nok.

Alternativer? `System.Random` er fin for grunnleggende behov, men det er ikke kryptografisk sterkt. Hvis du trenger noe sikrere, se på `System.Security.Cryptography.RandomNumberGenerator`. Det er litt mer tungvint å bruke, men mye sikrere:

```C#
using System.Security.Cryptography;

public class SecureRandomNumberGenerator
{
    static void Main(string[] args)
    {
        byte[] numbers = new byte[4];
        using (RandomNumberGenerator rng = RandomNumberGenerator.Create())
        {
            rng.GetBytes(numbers);
            int secureRandomNumber = BitConverter.ToInt32(numbers, 0);
            Console.WriteLine(secureRandomNumber);
        }
    }
}
```

Implementasjonsdetaljer å merke seg – `System.Random` er ikke trådsikker. Hvis flere tråder trenger tilfeldige tall samtidig, bør du synkronisere tilgangen eller bruke en trådsikker tilnærming.

## Se Også:
- Microsoft Docs for [`Random`](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-6.0) klassen.
- Microsoft Docs for [`RandomNumberGenerator`](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.randomnumbergenerator?view=net-6.0) klassen for kryptografisk bruk.
- En god forklaring på pseudotilfeldige tall vs. ekte tilfeldighet: [Random.org](https://www.random.org/randomness/).

Glem ikke, hva "tilfeldig" betyr i din kode avhenger av bruksscenariet ditt. Velg smart!
