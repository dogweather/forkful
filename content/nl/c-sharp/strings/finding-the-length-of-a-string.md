---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:08.685040-07:00
description: "Het vinden van een stringlengte betekent het tellen van de tekens. We\
  \ doen dit om invoer te valideren, door tekens te lopen, bronnen toe te wijzen,\
  \ of uit\u2026"
lastmod: '2024-03-13T22:44:50.801043-06:00'
model: gpt-4-0125-preview
summary: "Het vinden van een stringlengte betekent het tellen van de tekens. We doen\
  \ dit om invoer te valideren, door tekens te lopen, bronnen toe te wijzen, of uit\u2026"
title: De lengte van een string vinden
---

{{< edit_this_page >}}

## Wat & Waarom?

Het vinden van een stringlengte betekent het tellen van de tekens. We doen dit om invoer te valideren, door tekens te lopen, bronnen toe te wijzen, of uit simpele nieuwsgierigheid – de grootte kennen doet ertoe.

## Hoe te:

In C# geeft de eigenschap `string.Length` je het aantal tekens in een string. Zo gebruik je het:

```C#
using System;

class Program
{
    static void Main()
    {
        string example = "Hallo, Wereld!";
        Console.WriteLine(example.Length); // Uitvoer: 13
    }
}
```

Makkelijk, toch? Maar onthoud, het telt *tekens*, niet bytes. Met emoji's of speciale tekens, kunnen dingen ingewikkeld worden. Meer daarover later.

## Diepgaand

Historisch gezien was het vinden van de lengte van een string verbonden aan geheugenbeheer en manipulatie in programmering. Aangezien C# een hogere programmeertaal is, abstraheert het dat laag-niveau werk weg. Toch is het goed om te weten wat er onder de motorkap zit.

Alternatieven? Zeker! Je zou `example.ToCharArray().Length` in het wild kunnen tegenkomen, maar dat is slechts extra werk voor hetzelfde resultaat.

Nu, over die lastige tekens. C#'s `Length` eigenschap telt de `char` objecten van een string, elk die een UTF-16 code-eenheid vertegenwoordigt. Dat is prima totdat je *surrogaatparen* tegenkomt – karakters zoals emoji's die twee `char` objecten nodig hebben. Hier is het ding: `Length` telt die als twee. Ja.

Voor een nauwkeurige telling van *visuele* karakters of *grafemenclusters*, heb je de `StringInfo` klasse van System.Globalization nodig:

```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        string example = "👍"; // Duim omhoog emoji

        Console.WriteLine(example.Length); // Uitvoer: 2 <- Vanwege het surrogaatpaar!
        Console.WriteLine(new StringInfo(example).LengthInTextElements); // Uitvoer: 1
    }
}
```

Begrijp je het verschil? Het is niet alleen academisch; het zou de tekstverwerking op betekenisvolle manieren kunnen beïnvloeden.

## Zie Ook

Verken meer met deze bronnen:

- [Microsoft's officiële documentatie over strings](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- [Begrijpen van Unicode en UTF-16](https://unicodebook.readthedocs.io/unicode_encodings.html)
- [Documentatie van de StringInfo klasse](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.stringinfo?view=net-6.0)

Ken je strings, behandel ze verstandig, en schrijf code die telt – in elke zin.
