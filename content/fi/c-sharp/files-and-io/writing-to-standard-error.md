---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:48.274918-07:00
description: "Kuinka: C#:ssa vakiovirheeseen kirjoittamisen voi saavuttaa k\xE4ytt\xE4\
  m\xE4ll\xE4 `Console.Error`-virtaa. T\xE4t\xE4 virtaa k\xE4ytet\xE4\xE4n nimenomaan\
  \ virheviestien ja\u2026"
lastmod: '2024-03-13T22:44:56.590386-06:00'
model: gpt-4-0125-preview
summary: "C#:ssa vakiovirheeseen kirjoittamisen voi saavuttaa k\xE4ytt\xE4m\xE4ll\xE4\
  \ `Console.Error`-virtaa."
title: Kirjoittaminen standardivirheeseen
weight: 25
---

## Kuinka:
C#:ssa vakiovirheeseen kirjoittamisen voi saavuttaa käyttämällä `Console.Error`-virtaa. Tätä virtaa käytetään nimenomaan virheviestien ja diagnostiikan varten. Tässä on yksinkertainen esimerkki:

```csharp
Console.Error.WriteLine("Virhe: Pyyntöä ei voitu käsitellä.");
```

Esimerkkituloste (stderriin):
```
Virhe: Pyyntöä ei voitu käsitellä.
```

Skenaarioissa, joissa saatat käyttää kolmannen osapuolen kirjastoa, joka tarjoaa edistyneitä lokitustoimintoja, kuten `Serilog` tai `NLog`, voit määrittää nämä kirjastot kirjoittamaan virhelokit stderriin. Vaikka nämä esimerkit keskittyvät yksinkertaiseen konsolin uudelleenohjaukseen, muista, että tuotantosovelluksissa lokitusrakenteet tarjoavat paljon kestävämpiä virheenkäsittely- ja tulostusvaihtoehtoja. Tässä on yksinkertainen esimerkki `Serilog`:lla:

Ensimmäiseksi, asenna Serilog-paketti ja sen Console-säilö:

```
Install-Package Serilog
Install-Package Serilog.Sinks.Console
```

Sen jälkeen, määritä Serilog kirjoittamaan stderriin:

```csharp
using Serilog;

Log.Logger = new LoggerConfiguration()
    .WriteTo.Console(standardErrorFromLevel: Serilog.Events.LogEventLevel.Error)
    .CreateLogger();

Log.Information("Tämä on normaali viesti.");
Log.Error("Tämä on virheviesti.");
```

Esimerkkituloste (stderrille virheviestille):
```
[15:04:20 ERR] Tämä on virheviesti.
```

Huom: `standardErrorFromLevel`-määritys Serilogin konsolisäilössä ohjaa kaikki lokitapahtumat määritellyllä tasolla (tässä tapauksessa Virhe) tai korkeammalle vakiovirhevuo, kun taas alempitasoiset viestit kuten Tiedot kirjoitetaan vakiotulostevuo.
