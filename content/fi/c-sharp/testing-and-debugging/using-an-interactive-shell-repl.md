---
date: 2024-01-26 04:12:22.205288-07:00
description: "Kuinka: K\xE4ynnist\xE4 REPL C#-ymp\xE4rist\xF6ss\xE4si k\xE4ytt\xE4\
  m\xE4ll\xE4 C# Interactive -ikkunaa tai suorita `dotnet-script` terminaalissasi.\
  \ T\xE4ss\xE4 maistiainen sen k\xE4yt\xF6st\xE4."
lastmod: '2024-03-13T22:44:56.574606-06:00'
model: gpt-4-0125-preview
summary: "K\xE4ynnist\xE4 REPL C#-ymp\xE4rist\xF6ss\xE4si k\xE4ytt\xE4m\xE4ll\xE4\
  \ C# Interactive -ikkunaa tai suorita `dotnet-script` terminaalissasi."
title: "Interaktiivisen komentotulkin (REPL) k\xE4ytt\xF6"
weight: 34
---

## Kuinka:
Käynnistä REPL C#-ympäristössäsi käyttämällä C# Interactive -ikkunaa tai suorita `dotnet-script` terminaalissasi. Tässä maistiainen sen käytöstä:

```csharp
> var tervehdys = "Hei, REPL!";
> Console.WriteLine(tervehdys);
Hei, REPL!
>
```

Saat välittömästi palautetta. Ei kääntämistä ja suorittamista. Vain koodaa ja näe.

## Syväsukellus
REPL matkasi Lisp:stä moderneihin kieliin, menestyen dynaamisissa kielissä kuten Python. C#:ssa Roslyn toi REPL:n lähemmäs kehittäjiä. `csi` Roslynille ja `dotnet-script` .NET Corelle ovat vankkoja vaihtoehtoja. Syvempi leikkaus: ne arvioivat koodia rivi riviltä, ei kaikkea kerralla, mikä on erilainen suoritusmalli verrattuna tyypillisiin C#-sovelluksiin. Tämä vaikuttaa tilan säilymiseen suoritusten välissä ja muuttujien alueeseen.

Visual Studio'n C# Interactive -ikkuna on Roslynin voimin toimiva REPL. Siinä on IntelliSense, useita viittauksia ja NuGet-paketin tuki. Melkoinen askel verrattuna aikaisiin komentorivikokeiluihin.

Vaihtoehtoisille kielille, Python käyttää `IDLE`ä, JavaScriptilla on Node.js:n REPL ja F#:lla on `F# Interactive`. Jokainen edistää välittömiä palautelooppeja, jotka ovat arvokkaita testattaessa pieniä koodinpätkiä tai ymmärrettäessä kielen ominaisuuksia.

## Katso myös
- [.NET Core `dotnet-script` REPL](https://github.com/filipw/dotnet-script)
