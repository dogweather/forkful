---
aliases:
- /fi/c-sharp/rounding-numbers/
date: 2024-01-26 03:43:47.838751-07:00
description: "Numeroiden py\xF6rist\xE4minen tarkoittaa niiden s\xE4\xE4t\xE4mist\xE4\
  \ l\xE4himp\xE4\xE4n m\xE4\xE4riteltyyn paikkakerta-arvoon - ajattele niiden kiinnitt\xE4\
  mist\xE4 yksinkertaisempaan\u2026"
lastmod: 2024-02-18 23:09:07.615033
model: gpt-4-0125-preview
summary: "Numeroiden py\xF6rist\xE4minen tarkoittaa niiden s\xE4\xE4t\xE4mist\xE4\
  \ l\xE4himp\xE4\xE4n m\xE4\xE4riteltyyn paikkakerta-arvoon - ajattele niiden kiinnitt\xE4\
  mist\xE4 yksinkertaisempaan\u2026"
title: "Numerojen py\xF6rist\xE4minen"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Numeroiden pyöristäminen tarkoittaa niiden säätämistä lähimpään määriteltyyn paikkakerta-arvoon - ajattele niiden kiinnittämistä yksinkertaisempaan muotoon. Ohjelmoijat pyöristävät hallitakseen tarkkuutta, parantaakseen suorituskykyä tai näyttääkseen käyttäjäystävällisiä tuloksia - kuten hintoja, jotka eivät tarvitse kolmea desimaalipaikkaa.

## Kuinka:
Tässä on meno-paluulippu numeroiden pyöristämiseen C#-kielellä:

```csharp
using System;

public class RoundingExamples
{
    public static void Main()
    {
        double originalNumber = 123.4567;

        // Pyöristä lähimpään kokonaislukuun
        double rounded = Math.Round(originalNumber);
        Console.WriteLine(rounded); // Tuloste: 123

        // Määritä desimaalipaikkojen määrä
        double roundedTwoDecimalPlaces = Math.Round(originalNumber, 2);
        Console.WriteLine(roundedTwoDecimalPlaces); // Tuloste: 123.46

        // Pyöristä ylös riippumatta seuraavasta numerosta
        double roundedUp = Math.Ceiling(originalNumber);
        Console.WriteLine(roundedUp); // Tuloste: 124

        // Pyöristä alas riippumatta seuraavasta numerosta
        double roundedDown = Math.Floor(originalNumber);
        Console.WriteLine(roundedDown); // Tuloste: 123
    }
}
```

## Syväsukellus
Aikanaan pyöristäminen oli itsestäänselvyys laskentakustannusten karsimiseksi. Jokainen sykli laskettiin, ja numeroiden karsiminen säästi arvokasta aikaa. Aikaistetaan kelloa moderniin C#-kieleen, ja kyse on tuplasten ja desimaalien tunnetusti tarkkuusvirheisiin ja näyttöominaisuuksiin kallellaan olevan ominaisuuden hallinnasta.

`Math.Round`, `Math.Floor` ja `Math.Ceiling` -funktioiden ohella `MidpointRounding`-luettelointityyppi antaa meidän määrätä parkkipaikalla seisovien onnettarien kohtalon - se on risteyskohta pankkisääntöjen ja "pyöristä puoli ylös" -leikkikentän oikeudenmukaisuuden välillä.

Kovemmille yleisöille, kuten vakaville matematiikka- tai rahoitussovelluksille, meillä on `decimal` `double`:n sijaan, mikä vähentää pyöristämisdraamaa tarjoamalla suurempaa tarkkuutta - vähemmän pyöristämistä, vähemmän ongelmia.

## Katso myös
- [Viralliset C#-asiakirjat `Math.Round`-komennosta](https://docs.microsoft.com/en-us/dotnet/api/system.math.round)
- [Stack Overflow: Milloin minun pitäisi käyttää Doublea Decimalin sijaan?](https://stackoverflow.com/questions/1165761/decimal-vs-double-which-one-should-i-use-and-when)
- [IEEE-standardi liukulukuaritmetiikalle (IEEE 754)](https://en.wikipedia.org/wiki/IEEE_754)
