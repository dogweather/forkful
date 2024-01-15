---
title:                "Sammenkobling av strenger"
html_title:           "C#: Sammenkobling av strenger"
simple_title:         "Sammenkobling av strenger"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere eller "konkatinere" strenger i C# er en viktig del av å utvikle effektive og strukturerte programmer. Det lar deg enkelt kombinere tekst og variabler for å opprette dynamiske setninger og meldinger.

## Slik gjør du det

For å konkatinere strenger i C#, bruker man pluss-operatøren ("+"). Dette legger sammen de ulike strengene og variablene til en ny streng. Se et eksempel nedenfor:

```C#
string fornavn = "Jens";
string etternavn = "Hansen";
string navn = fornavn + " " + etternavn;
Console.WriteLine(navn);
```

**Output:** "Jens Hansen"

Her ser vi at vi har kombinert de to variablene `fornavn` og `etternavn` for å lage en ny streng `navn`. I denne setningen brukte vi også et mellomrom ved å sette inn en tom streng mellom variablene.

## Dykk dypere

I C# kan du også bruke `string.Format` metoden for å konkatinere strenger. Dette lar deg forhåndsdefinere en setning med plassholdere for variabler, som senere kan bli erstattet med riktige verdier. Se et eksempel nedenfor:

```C#
string fornavn = "Jens";
string etternavn = "Hansen";
string by = "Oslo";
string setning = string.Format("{0} {1} kommer fra {2}.", fornavn, etternavn, by);
Console.WriteLine(setning);
```

**Output:** "Jens Hansen kommer fra Oslo."

I tillegg til `string.Format` metoden, kan du også bruke `StringBuilder` klassen for å bygge komplekse strenger. Dette kan være nyttig hvis du trenger å gjøre mange konkatineringer i en løkke eller i en større operasjon.

## Se også

- Microsoft dokumentasjon for strenger i C#: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/string
- Norsk C# forum: https://www.norwegian-csharp.com/