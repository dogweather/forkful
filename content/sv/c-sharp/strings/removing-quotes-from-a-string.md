---
date: 2024-01-26 03:39:19.685401-07:00
description: "Att ta bort citationstecken fr\xE5n en str\xE4ng i C# inneb\xE4r att\
  \ du tar bort de irriterande dubbla (`\"`) eller enkla (`'`) citationstecken som\
  \ omsluter din\u2026"
lastmod: '2024-03-13T22:44:37.901135-06:00'
model: gpt-4-0125-preview
summary: "Att ta bort citationstecken fr\xE5n en str\xE4ng i C# inneb\xE4r att du\
  \ tar bort de irriterande dubbla (`\"`) eller enkla (`'`) citationstecken som omsluter\
  \ din text."
title: "Ta bort citattecken fr\xE5n en str\xE4ng"
weight: 9
---

## Hur man gör:
```csharp
string withQuotes = "\"Hej, världen!\"";
Console.WriteLine($"Original: {withQuotes}");

// Ta bort dubbla citationstecken
string withoutDoubleQuotes = withQuotes.Replace("\"", "");
Console.WriteLine($"Utan Dubbla Citationstecken: {withoutDoubleQuotes}");

// Ta bort enkla citationstecken (utgår från att din sträng hade det från början)
string withSingleQuotes = "'Hej, världen!'";
string withoutSingleQuotes = withSingleQuotes.Replace("'", "");
Console.WriteLine($"Utan Enkla Citationstecken: {withoutSingleQuotes}");
```

Utdata:
```
Original: "Hej, världen!"
Utan Dubbla Citationstecken: Hej, världen!
Utan Enkla Citationstecken: Hej, världen!
```

## Fördjupning
Konceptet med att ta bort citationstecken är inte nytt eller särskilt komplext, men det är avgörande eftersom citationstecken ofta används för att avgränsa strängar. När en sträng med okodade citattecken inkluderas i en kodblock eller en datafil kan det avsluta strängen för tidigt, vilket orsakar fel eller säkerhetsproblem som injektionsattacker.

Historiskt sett har hantering av citationstecken varit en del av validerings- och saneringsprocessen i datahantering. Även om `.Replace()`-metoden är okomplicerad för att dra ut citattecken från en enkel sträng, kan du behöva använda mer avancerade tekniker som reguljära uttryck för att hantera mer komplexa scenarier, som inbäddade citationstecken eller villkorlig borttagning.

Alternativ till `.Replace()` inkluderar metoder från `Regex`-klassen när du behöver finjusterad kontroll eller har att göra med mönster snarare än fasta tecken. Till exempel kan `Regex.Unescape()` vara praktiskt när du hanterar flyktade tecken.

När det gäller implementering, kom ihåg att strängar i C# är oföränderliga, vilket innebär att varje gång du använder `.Replace()`, skapas en ny sträng. Detta är inte något stort problem för små eller engångsoperationer, men något att tänka på ur ett prestandaperspektiv för stora eller många strängar.

## Se även:
- [Dokumentation för String.Replace-metoden](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netframework-4.8)
- [Reguljära uttryck i .NET](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- [Bästa praxis för säker hantering av strängar](https://www.owasp.org/index.php/Data_Validation)
