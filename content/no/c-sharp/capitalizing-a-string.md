---
title:                "Store bokstaver i en streng"
html_title:           "C#: Store bokstaver i en streng"
simple_title:         "Store bokstaver i en streng"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

"## Hvorfor"

Det å gjøre strenger til store bokstaver kan være nødvendig for å skape et visuelt skille mellom ulike deler av tekst, eller for å sikre at data samstemmer med forventningene. Dette kan være spesielt nyttig i situasjoner hvor man jobber med brukerinputs eller sammenligninger mellom strenger.

## Hvordan gjøres det

For å konvertere en streng til store bokstaver i C#, kan man bruke den innebygde metoden `ToUpper()`. Denne metoden tar ingen parametere og vil returnere en kopi av den opprinnelige strengen, men med alle bokstavene i store bokstaver.

```C#
string navn = "ola nordmann";
string storNavn = navn.ToUpper();
Console.WriteLine(storNavn);
```
Output: OLA NORDMANN

Man kan også benytte seg av metoden `ToUpperInvariant()`, som fungerer på samme måte som `ToUpper()` men gir en mer pålitelig og konsistent konvertering mellom forskjellige kulturer.

## Dypdykk

Når man bruker `ToUpper()` og `ToUpperInvariant()` blir det laget en kopi av den opprinnelige strengen, og den originale strengen forblir den samme. Dette betyr at dersom man ønsker å endre den originale strengen til å være i store bokstaver, må man tildele den konverterte strengen tilbake til variabelen.

Det finnes også andre måter å gjøre en streng til store bokstaver i C#, som for eksempel å bruke `string.ToUpper(CultureInfo)`. Dette gir mer kontroll over hvordan bokstavene blir konvertert og kan være nyttig i spesifikke situasjoner.

Det er viktig å merke seg at både `ToUpper()` og `ToUpperInvariant()` kun konverterer bokstaver som har en entydig stor bokstav. Dette betyr at noen spesialtegn, slik som Æ, Ø og Å, kan bli konvertert til feil bokstav i enkelte tilfeller. For å unngå dette kan man bruke metoden `String.Normalize()` for å sikre at spesialtegnene blir riktig konvertert.

## Se også

- [Microsoft Docs: String.ToUpper()](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper)
- [Microsoft Docs: String.ToUpperInvariant()](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupperinvariant)
- [Microsoft Docs: CultureInfo](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo)
- [Microsoft Docs: String.Normalize()](https://docs.microsoft.com/en-us/dotnet/api/system.string.normalize)