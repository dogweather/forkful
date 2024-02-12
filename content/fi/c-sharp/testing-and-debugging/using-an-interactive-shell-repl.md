---
title:                "Interaktiivisen komentotulkin (REPL) käyttö"
aliases: - /fi/c-sharp/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:12:22.205288-07:00
model:                 gpt-4-0125-preview
simple_title:         "Interaktiivisen komentotulkin (REPL) käyttö"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
REPL eli Lue-Arvo-Tulosta Silmukka antaa sinun kirjoittaa C#-koodia ja suorittaa sitä vuorovaikutteisesti. Ohjelmoijat käyttävät sitä nopeisiin kokeiluihin, virheenkorjaukseen tai C#:n opiskeluun ilman täydellisten projektien pystyttämisen vaivaa.

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
