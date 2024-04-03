---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:45.490488-07:00
description: "\xC5 skrive til standardfeil (stderr) i C# inneb\xE6rer \xE5 dirigere\
  \ feilmeldinger og diagnostikk separat fra vanlig utdata (stdout) for \xE5 hjelpe\
  \ brukere og\u2026"
lastmod: '2024-03-13T22:44:40.814143-06:00'
model: gpt-4-0125-preview
summary: "\xC5 skrive til standardfeil (stderr) i C# inneb\xE6rer \xE5 dirigere feilmeldinger\
  \ og diagnostikk separat fra vanlig utdata (stdout) for \xE5 hjelpe brukere og utviklere\
  \ til \xE5 skille mellom normalt programutdata og feilnotifikasjoner."
title: Skriving til standardfeil
weight: 25
---

## Hvordan:
I C# kan skriving til standardfeil oppnås ved å bruke `Console.Error`-strømmen. Denne strømmen brukes spesifikt for feilmeldinger og diagnostikk. Her er et grunnleggende eksempel:

```csharp
Console.Error.WriteLine("Feil: Kunne ikke behandle forespørselen.");
```

Eksempel på utdata (til stderr):
```
Feil: Kunne ikke behandle forespørselen.
```

For scenarioer hvor du kanskje bruker et tredjepartsbibliotek som tilbyr avanserte loggefunksjoner, som `Serilog` eller `NLog`, kan du konfigurere disse bibliotekene til å skrive feillogger til stderr. Mens disse eksemplene fokuserer på enkel konsollomdirigering, husk at i produksjonsapplikasjoner tilbyr loggingsrammeverk mye mer robust feilhåndtering og utdataopsjoner. Her er et enkelt eksempel med `Serilog`:

Først, installer Serilog-pakken og dens Console-sink:

```
Install-Package Serilog
Install-Package Serilog.Sinks.Console
```

Deretter, konfigurer Serilog for å skrive til stderr:

```csharp
using Serilog;

Log.Logger = new LoggerConfiguration()
    .WriteTo.Console(standardErrorFromLevel: Serilog.Events.LogEventLevel.Error)
    .CreateLogger();

Log.Information("Dette er en vanlig melding.");
Log.Error("Dette er en feilmelding.");
```

Eksempel på utdata (til stderr for feilmeldingen):
```
[15:04:20 ERR] Dette er en feilmelding.
```

Merk: `standardErrorFromLevel`-konfigurasjonen i Serilog sin konsoll-sink omdirigerer alle loggehendelser på det angitte nivået (Error, i dette tilfellet) eller høyere til standardfeilstrømmen, mens meldinger på et lavere nivå som Informasjon skrives til standard utstrøm.
