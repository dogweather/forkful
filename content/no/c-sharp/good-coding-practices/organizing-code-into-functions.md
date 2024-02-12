---
title:                "Organisering av kode i funksjoner"
aliases:
- /no/c-sharp/organizing-code-into-functions.md
date:                  2024-01-26T01:09:23.840515-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organisering av kode i funksjoner"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å organisere kode i funksjoner er som å sortere LEGO-klosser i bokser – det gjør det enklere å finne og bruke dem. Vi gjør dette for å unngå gjentakelse, for å forenkle forståelsen og for å gjøre vedlikeholdet mindre hodebry.

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
