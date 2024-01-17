---
title:                "Konvertere en streng til små bokstaver"
html_title:           "C#: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Konvertering av strenger til små bokstaver handler om å gjøre alle bokstaver i en tekst eller streng om til små bokstaver. Dette gjør det enklere for programmerere å behandle og sammenligne tekst, da store og små bokstaver vanligvis behandles forskjellig i datamaskiner. 

## Hvordan:

Konvertering av en tekst til små bokstaver kan enkelt gjøres ved å bruke .ToLower() metoden i C#, slik som vist i eksempelet nedenfor:

```C#
string tekst = "Dette er EN tekst";
Console.WriteLine(tekst.ToLower());
```
Dette vil gi følgende resultat: "dette er en tekst"

Det er også mulig å konvertere enkelte bokstaver til små bokstaver ved å bruke .ToLower() på individuelle bokstaver i strengen.

## Dypdykk:

Historisk sett har det vært viktig å kunne skille mellom store og små bokstaver i datamaskiner, da noen systemer kun kunne håndtere en type bokstaver. Med moderne datamaskiner er dette ikke lenger et problem, men det er fortsatt viktig å være bevisst på forskjellen mellom store og små bokstaver ved sammenligning av tekst. 

En alternativ metode for å konvertere tekst til små bokstaver er å bruke .ToUpper() metoden, som gjør det motsatte - konverterer alle bokstaver til store bokstaver. Dette kan være nyttig i visse situasjoner.

Implementeringen av .ToLower() metoden i C# er basert på Unicode standarden for store og små bokstaver. Dette betyr at den vil fungere for alle språk og bokstaver, ikke bare det engelske alfabetet.

## Se også:

- https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/lowercase-comparison
- https://en.wikipedia.org/wiki/Unicode_equivalence#Operations_on_text_ignoring_case