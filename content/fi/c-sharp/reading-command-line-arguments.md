---
title:                "C#: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi
Onko sinulla koskaan ollut tarvetta lukea komentoriviparametreja C# -ohjelmassa? Tässä blogikirjoituksessa näytämme, miten se tehdään.

## Kuinka
```C#
using System;

class CommandArgsExample
{
  static void Main(string[] args)
  {
    // Tulostaa komentoriviparametrit
    foreach (string arg in args)
    {
      Console.WriteLine(arg);
    }
  }
}
```

Kun suoritat tämän esimerkkikoodin komentoriviltä, tulostuu komentoriviparametrit ja niiden arvot.

```
> dotnet CommandArgsExample.dll hello world
hello
world
```

## Syvällinen sukellus
Kommentoimme koodin selittääksemme, mitä siinä tapahtuu. Komentoriviparametri voidaan lukea myös suoraan tietyssä indeksissä, jos tiedetään sen paikka:

```C#
// Tulostaa toisen komentoriviparametrin
Console.WriteLine(args[1]);
```

Tämä on erityisen hyödyllistä, jos ohjelmallesi on pakko antaa tiettyjä parametreja ja haluat varmistaa, että ne annetaan oikeassa järjestyksessä.

## Katso myös
- [Microsoftin C# -dokumentaatio komentoriviparametrien lukemisesta](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [Stack Overflow -vastaus C# -komentoriviparametrien lukemisesta](https://stackoverflow.com/questions/5598462/how-to-read-command-line-arguments-in-c-sharp)