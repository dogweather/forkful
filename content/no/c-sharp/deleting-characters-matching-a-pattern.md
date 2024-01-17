---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "C#: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sletting av tegn som matcher et mønster er en vanlig oppgave for programvareutviklere. Dette er når vi søker gjennom en streng med tekst og fjerner alle forekomster av et bestemt tegnmønster. Dette kan være nyttig for å formatere tekst eller behandle data på en spesifikk måte.

## Hvordan:
Du kan enkelt utføre sletting av tegn som matcher et mønster ved å bruke metoder fra C#-biblioteket Regex. Nedenfor er et eksempel på hvordan du kan slette alle tall fra en streng:

```C#
string tekst = "Jeg har 123 epler";
string nyTekst = Regex.Replace(tekst, "[0-9]", "");
Console.WriteLine(nyTekst);
```

Dette vil resultere i følgende utdata:

```C#
Jeg har epler
```

## Dykk dypere:
Sletting av tegn som matcher et mønster har blitt brukt siden de tidlige dagene av dataprogrammering, da personer måtte gjøre dette manuelt. I dag er dette en standardfunksjon i de fleste programmeringsspråk, og det finnes også alternativer som LINQ-metoder for å utføre samme oppgave.

Når du implementerer sletting av tegn i ditt eget program, må du være forsiktig med hvilke tegn du sletter, da dette kan påvirke teksten eller dataene på uønskede måter.

## Se også:
- [Microsoft C# Regex klasse](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=netstandard-2.0)
- [LINQ-metoder for å utføre sletting av tegn](https://docs.microsoft.com/en-us/dotnet/api/system.linq.enumerable.where?view=netstandard-2.1)
- [En oversikt over regulære uttrykk](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions-overview)