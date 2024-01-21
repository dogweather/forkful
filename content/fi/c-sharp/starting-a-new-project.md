---
title:                "Uuden projektin aloittaminen"
date:                  2024-01-20T18:03:14.329355-07:00
model:                 gpt-4-1106-preview
simple_title:         "Uuden projektin aloittaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? / Mitä & Miksi?
Uuden projektin aloittaminen tarkoittaa uuden sovelluksen tai ohjelmiston kehitysvaiheen aloittamista. Ohjelmoijat tekevät sen, jotta voivat ratkaista ongelmia, toteuttaa ideoita tai vastata kysyntään.

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