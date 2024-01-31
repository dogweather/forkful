---
title:                "Kirjoittaminen vakiovirheeseen"
date:                  2024-01-19
simple_title:         "Kirjoittaminen vakiovirheeseen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Standard error (stderr) on tietokoneen tapa raportoida ohjelman virheitä. Ohjelmoijat käyttävät sitä erottamaan virheviestit normaalista tulosteesta, mikä helpottaa logien analysointia ja vianetsintää.

## How to:
C#:ssa `Console.Error` on yleisin tapa kirjoittaa stderr-virtaan. Tässä esimerkki:

```C#
using System;

class StderrExample
{
    static void Main()
    {
        Console.Error.WriteLine("Tämä on virheviesti!");
        Console.WriteLine("Tämä on normaali tuloste.");
    }
}
```

Kun ajat edellisen ohjelman, näet:

```
Tämä on normaali tuloste.
```

Virheviestit eivät näy konsoleilla oletusarvoisesti, mutta voit ohjata ne tiedostoon tai toiseen ulostuloon komentoriviltä.

## Deep Dive
Historiallisesti standard error erotettiin standarditulosteesta (stdout), jotta virheviestit pysyvät näkyvissä, vaikka normaali tuloste ohjattaisiin muualle. C#:ssa `Console.Error` on `TextWriter`-objekti, ja voit vaihtaa kohdetta metodilla `Console.SetError`. Vaihtoehdot ovat kirjoittaa manuaalisesti tiedostoon tai käyttää logituskehyksiä (esim. NLog, Serilog), jotka tarjoavat enemmän toiminnallisuutta.

## See Also
- Microsoft Docs, Console.Error: https://docs.microsoft.com/en-us/dotnet/api/system.console.error
- Microsoft Docs, Console.SetError: https://docs.microsoft.com/en-us/dotnet/api/system.console.seterror
- Ohjeita virheenkirjaukseen .NET:ssä: https://serilog.net/
- NLog, eräs .NET-kirjauskirjasto: https://nlog-project.org/
