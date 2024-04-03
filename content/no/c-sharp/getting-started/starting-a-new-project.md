---
date: 2024-01-20 18:03:21.458660-07:00
description: "Slik gj\xF8r du: For \xE5 starte et nytt prosjekt, kan du bruke .NET\
  \ CLI (Command Line Interface) eller Visual Studio. La oss kj\xF8re gjennom CLI."
lastmod: '2024-03-13T22:44:40.796403-06:00'
model: gpt-4-1106-preview
summary: "For \xE5 starte et nytt prosjekt, kan du bruke .NET CLI (Command Line Interface)\
  \ eller Visual Studio."
title: "\xC5 starte et nytt prosjekt"
weight: 1
---

## Slik gjør du:
For å starte et nytt prosjekt, kan du bruke .NET CLI (Command Line Interface) eller Visual Studio. La oss kjøre gjennom CLI:

```C#
// 1. Åpne terminal og skriv inn:
dotnet new console -o MyNewProject

// 2. Naviger til prosjektmappen:
cd MyNewProject

// 3. Skriv inn følgende kode i Program.cs:
using System;

namespace MyNewProject
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Hei, Norge!");
        }
    }
}

// 4. Kjør prosjektet med:
dotnet run
```

Kjører du koden over, vil du se:
```
Hei, Norge!
```

## Dypdykk:
C#-prosjekter har vært en del av utvikleres liv siden .NET-plattformen ble lansert i 2002. Før CLI ble populært, støttet Visual Studio opprettelsen av nye prosjekter med en grafisk tilnærming - noe som fortsatt er vanlig. Alternativer for å starte nye prosjekter inkluderer å bruke IDEer som JetBrains Rider eller Visual Studio Code med passende utvidelser.

C# og .NET har vært gjennom mange iterasjoner. Fra .NET Framework til .NET Core og nå til .NET 5/6 og fremover, måten vi oppretter prosjekter på har blitt forenklet betydelig. .NET CLI er idag et kraftfullt verktøy for å administrere prosjekter, håndtere avhengigheter og kjøre applikasjoner.

Når du oppretter et prosjekt med `dotnet new`, er det en rekke maler tilgjengelig. For eksempel kan `console` byttes ut med `webapp` for å lage en ASP.NET Core webapplikasjon eller `classlib` for å lage et klassebibliotek.

.NET 5 og 6 har introdusert ny funksjonalitet for å forbedre ytelsen og produktiviteten. For eksempel, med top-level statements trenger du ikke lenger `namespace` eller `class` deklarasjoner i enkle programmer, noe som forenkler kodebasen ytterligere.

## Se også:
- Microsofts offisielle dokumentasjon for .NET CLI: https://docs.microsoft.com/dotnet/core/tools/
- Oversikt over C# prosjektstruktur: https://docs.microsoft.com/dotnet/core/tutorials/
- Lær mer om .NET versjonshistorikk: https://dotnet.microsoft.com/platform/dotnet-standard#versions
- Om top-level statements: https://docs.microsoft.com/dotnet/csharp/fundamentals/program-structure/top-level-statements
