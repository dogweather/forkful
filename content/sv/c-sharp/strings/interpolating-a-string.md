---
changelog:
- 2024-02-25, gpt-4-0125-preview, translated from English
date: 2024-02-25 17:07:02.554791-07:00
description: "Hur g\xF6r man: I C# anges str\xE4nginterpolering med ett dollartecken\
  \ (`$`) f\xF6ljt av en str\xE4ngliteral. Variabelnamn eller uttryck omsluts av m\xE5\
  svingar (`{}`)."
lastmod: '2024-03-13T22:44:37.899243-06:00'
model: gpt-4-0125-preview
summary: "I C# anges str\xE4nginterpolering med ett dollartecken (`$`) f\xF6ljt av\
  \ en str\xE4ngliteral."
title: "Interpolering av en str\xE4ng"
weight: 8
---

## Hur gör man:
I C# anges stränginterpolering med ett dollartecken (`$`) följt av en strängliteral. Variabelnamn eller uttryck omsluts av måsvingar (`{}`).

```csharp
string name = "Jane";
int age = 28;
string interpolatedString = $"Hej, {name}! Du är {age} år gammal.";
Console.WriteLine(interpolatedString);
// Utskrift: Hej, Jane! Du är 28 år gammal.
```

I ett mer komplext exempel kan du utföra operationer eller anropa metoder inuti måsvingarna:

```csharp
double price = 19.99;
int quantity = 3;
string orderDetail = $"Totalt pris: {price * quantity:C2}";
Console.WriteLine(orderDetail);
// Utskrift: Totalt pris: $59.97
```
Format-specifikatorn `:C2` inuti måsvingarna formaterar numret som en valuta med två decimaler.

För scenarier som kräver mer avancerad formatering eller lokalisering kan du överväga att använda metoden `string.Format` eller bibliotek som Humanizer. Humanizer kan manipulera och visa strängar, datum, tider, tidsintervaller, nummer och kvantiteter i ett mer läsbart format. Nedan följer ett exempel på användning av Humanizer för komplex strängmanipulering. Notera att Humanizer inte är en del av .NET-standardsbiblioteket och kräver installation av NuGet-paketet `Humanizer`.

Först, installera Humanizer via NuGet:

```
Install-Package Humanizer
```

Sedan kan du använda det som följer:

```csharp
using Humanizer;

int dayDifference = 5;
string humanized = $"Händelsen var {dayDifference} dagar sedan.".Humanize();
Console.WriteLine(humanized);
// Beroende på konfiguration och kultur, en möjlig utskrift: Händelsen var 5 dagar sedan.
```

Detta exempel demonstrerar grundläggande användning. Humanizer stöder ett brett utbud av funktionaliteter som kan appliceras på strängar, datum, nummer och mer, vilket gör dina applikationer mer tillgängliga och intuitiva.
