---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Generera Slumpmässiga Nummer i C#: En Praktisk Guide

## Vad & Varför?
Att generera slumpmässiga nummer innebär att skapa nummer utan något förutsägbart mönster. Programmerare gör detta för att till exempel simulera data, testa program och skapa gameplay i spel.

## Hur man gör:
Här är ett exempel på hur man genererar ett slumpmässigt nummer i C# mellan 0 och 100:

```C#
using System;
class Program
{
    static void Main()
    {
        Random random = new Random();
        int number = random.Next(0, 101);
        Console.WriteLine(number);
    }
}
```
När du kör detta program kommer utmatningen att vara ett slumpmässigt nummer mellan 0 och 100.

## Djupdykning
### Historisk kontext
Slumpmässiga tal har en lång historia inom datorprogrammering och datavetenskap. Datorer använder inte verkligt slumpmässiga tal, utan pseudoslumpmässiga, vilket innebär att de genereras med en deterministisk algoritm.

### Alternativ
Om du behöver en mer robust lösning för statiska applikationer, kan du titta på RNGCryptoServiceProvider, som ger kryptografisk säkerhet.

```C#
using System;
using System.Security.Cryptography;
class Program
{
    static void Main()
    {
        using RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider();
        byte[] randomBytes = new byte[4];
        rng.GetBytes(randomBytes);
        int number = BitConverter.ToInt32(randomBytes, 0);
        Console.WriteLine(number);
    }
}
```

### Implementeringsdetaljer
Observera att både Random och RNGCryptoServiceProvider klasserna i C# använder en sådd för att generera talserier. Om du använder samma sådd kommer du att få samma serie.

## Se även
[Hur man genererar ett säkert slumpmässigt nummer](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider)

[Så här genereras pseudoslumpmässiga tal i .NET](https://docs.microsoft.com/en-us/dotnet/api/system.random)

[Mer detaljerade information om 'Random' klassen](https://docs.microsoft.com/en-us/dotnet/api/system.random)