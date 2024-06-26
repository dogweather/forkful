---
date: 2024-01-20 18:03:14.329355-07:00
description: "How to: / Kuinka: Aloitetaan uusi C#-projekti Visual Studio 2022:ssa.\
  \ Ei mit\xE4\xE4n ylim\xE4\xE4r\xE4ist\xE4."
lastmod: '2024-04-05T22:38:57.179594-06:00'
model: gpt-4-1106-preview
summary: "/ Kuinka: Aloitetaan uusi C#-projekti Visual Studio 2022:ssa. Ei mit\xE4\
  \xE4n ylim\xE4\xE4r\xE4ist\xE4."
title: Uuden projektin aloittaminen
weight: 1
---

## How to: / Kuinka:
Aloitetaan uusi C#-projekti Visual Studio 2022:ssa. Ei mitään ylimääräistä.

```C#
using System;

namespace HelloWorld
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Moi maailma!");
        }
    }
}
```

Kun koodi ajetaan, konsoliin tulostuu:

```
Moi maailma!
```

## Deep Dive / Syväsukellus:
C# kehittyi Microsoftin alustalla 2000-luvun alussa osana .NET Frameworkia. Se tarjoaa yksinkertaisuuden ja voiman balanssin, sopien sekä pieniin että monimutkaisiin projekteihin. Nykyään .NET 6:n ja C# 10:n myötä on entistä helppo aloittaa uusi projekti komentoriviltä `dotnet new` -komennolla. 

Vaihtoehtoina on vaikkapa .NET Core, vanhemmat .NET Framework -versiot tai erilaiset kehitysympäristöt kuten JetBrains Rider. Kehitystyökalut ovat monipuolistuneet ja erikoistuneet eri käyttötapauksiin. 

Uuden projektin aloittamisessa olennaista on .csproj-tiedosto, joka hallinnoi projektirakennetta ja riippuvuuksia. Käyttäen SDK:n tyylin .csproj-tiedostoja, .NET 6:ssa on siirrytty kohti entistä tiiviimpää ja yksinkertaisempaa projektien määrittelyä.

## See Also / Katso Myös:
- [Microsoft C#-dokumentointi](https://docs.microsoft.com/fi-fi/dotnet/csharp/)
- [.NET 6 -uutuudet](https://docs.microsoft.com/fi-fi/dotnet/core/whats-new/dotnet-6)
- [Visual Studio 2022](https://visualstudio.microsoft.com/vs/)
- [JetBrains Rider](https://www.jetbrains.com/rider/)
