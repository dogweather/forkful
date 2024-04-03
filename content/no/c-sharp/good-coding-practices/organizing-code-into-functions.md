---
date: 2024-01-26 01:09:23.840515-07:00
description: 'Hvordan: Forestil deg at du har kode som skriver ut en hilsen flere
  ganger. Uten funksjoner blir det rot. Med funksjoner blir det ryddig.'
lastmod: '2024-03-13T22:44:40.801493-06:00'
model: gpt-4-1106-preview
summary: Forestil deg at du har kode som skriver ut en hilsen flere ganger.
title: Organisering av kode i funksjoner
weight: 18
---

## Hvordan:
Forestil deg at du har kode som skriver ut en hilsen flere ganger. Uten funksjoner blir det rot. Med funksjoner blir det ryddig.

```C#
// Uten funksjoner - gjentagende
Console.WriteLine("Hei, Amy!");
Console.WriteLine("Hei, Bob!");
Console.WriteLine("Hei, Charlie!");

// Med funksjoner - renere
void Hilsen(string navn) {
    Console.WriteLine($"Hei, {navn}!");
}

Hilsen("Amy");
Hilsen("Bob");
Hilsen("Charlie");
```

Resultatet er det samme, men den andre versjonen er mye ryddigere.

## Dypdykk
Langt tilbake, i assembler-språkdager, ville man hoppe til forskjellige deler av koden med GOTO – kaotisk og vanskelig å spore. Funksjoner er et stort steg opp, som organiserte skuffer i en verktøykasse. Alternativer? Klart det. Du har metoder, som er funksjoner i en klassekontekst. Så er det lambdas og innebygde funksjoner for raske, engangsoppgaver.

Når det gjelder implementering – små, fokuserte funksjoner er gull. De er enklere å teste og feilsøke. Store funksjoner med mange ansvarsområder kan bli monstrøse, og få den tvilsomme tittelen "spagettikode". Hold deg til én jobb per funksjon; du vil takke deg selv senere.

## Se Også
For mer om funksjoner og beste praksiser, sjekk ut:

- Clean Code av Robert C. Martin: Prinsipper for å holde funksjonene dine ryddige.
- Refactoring av Martin Fowler: Måter å forbedre eksisterende kode på.
- Microsoft C# Guide om Metoder: https://docs.microsoft.com/no-no/dotnet/csharp/programming-guide/classes-and-structs/methods
