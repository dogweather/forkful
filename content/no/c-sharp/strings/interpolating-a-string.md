---
changelog:
- 2024-02-25, gpt-4-0125-preview, translated from English
date: 2024-02-25 17:07:03.825592-07:00
description: "Strenginterpolering i C# lar deg opprette en ny streng ved \xE5 inkludere\
  \ uttrykk inni en strengbokstavelig, noe som gj\xF8r det enklere \xE5 formatere\
  \ og\u2026"
lastmod: '2024-03-11T00:14:14.338253-06:00'
model: gpt-4-0125-preview
summary: "Strenginterpolering i C# lar deg opprette en ny streng ved \xE5 inkludere\
  \ uttrykk inni en strengbokstavelig, noe som gj\xF8r det enklere \xE5 formatere\
  \ og\u2026"
title: Interpolering av en streng
---

{{< edit_this_page >}}

## Hva og hvorfor?
Strenginterpolering i C# lar deg opprette en ny streng ved å inkludere uttrykk inni en strengbokstavelig, noe som gjør det enklere å formatere og konkatere strenger. Programmerere bruker denne funksjonen for å forbedre kodelesbarheten og vedlikeholdbarheten, spesielt når de håndterer dynamisk strenginnhold.

## Hvordan:
I C#, indikeres strenginterpolering med et dollartegn (`$`) etterfulgt av en strengbokstavelig. Variabelnavnene eller uttrykkene er omsluttet med krøllparenteser (`{}`).

```csharp
string name = "Jane";
int age = 28;
string interpolatedString = $"Hei, {name}! Du er {age} år gammel.";
Console.WriteLine(interpolatedString);
// Utskrift: Hei, Jane! Du er 28 år gammel.
```

I et mer komplekst eksempel kan du utføre operasjoner eller kalle metoder innenfor krøllparentesene:

```csharp
double price = 19.99;
int quantity = 3;
string orderDetail = $"Totalpris: {price * quantity:C2}";
Console.WriteLine(orderDetail);
// Utskrift: Totalpris: $59.97
```
Formatbestemmeren `:C2` inni krøllparentesene formaterer tallet som en valuta med to desimaler.

For scenarier som krever mer avansert formatering eller lokalisering, kan du vurdere å bruke `string.Format`-metoden eller biblioteker som Humanizer. Humanizer kan manipulere og vise strenger, datoer, tider, tidsintervaller, tall og mengder i et mer lesbart format for mennesker. Nedenfor er et eksempel på bruk av Humanizer for kompleks strengmanipulering. Merk at Humanizer ikke er en del av .NET standard biblioteket og krever installasjon av NuGet-pakken `Humanizer`.

Først, installer Humanizer via NuGet:

```
Install-Package Humanizer
```

Deretter kan du bruke det slik:

```csharp
using Humanizer;

int dayDifference = 5;
string humanized = $"Arrangementet var for {dayDifference} dager siden.".Humanize();
Console.WriteLine(humanized);
// Avhengig av konfigurasjonen og kulturen, en mulig utskrift: Arrangementet var for 5 dager siden.
```

Dette eksempelet demonstrerer basisbruk. Humanizer støtter et bredt spekter av funksjonaliteter som kan brukes på strenger, datoer, tall og mer, noe som gjør applikasjonene dine mer tilgjengelige og intuitive.
