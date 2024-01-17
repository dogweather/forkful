---
title:                "Søk og erstatning av tekst"
html_title:           "C#: Søk og erstatning av tekst"
simple_title:         "Søk og erstatning av tekst"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Søk og erstatt av tekst er en vanlig oppgave for programmerere. Ved å bruke kode kan du automatisk finne spesifikk tekst og erstatte den med ønsket tekst. Dette sparer tid og sikrer nøyaktighet i store programmeringsprosjekter.

## Slik gjør du:

Å søke og erstatte tekst i C# er enkelt med innebygde funksjoner og metodene. For å søke og erstatte i en tekststreng kan du bruke `string.Replace()` metoden. For eksempel:

```
string text = "Hei verden!";
string newText = text.Replace("Hei", "Hallo");
Console.WriteLine(newText);
```

Dette vil erstatte "Hei" med "Hallo" i tekststrengen og printe ut "Hallo verden!".

For å søke og erstatte i hele prosjekter, kan du bruke Regex (regulære uttrykk). Dette er nyttig når du ønsker å gjøre søk og erstatte handlinger basert på spesifikke mønstre. For eksempel:

```
using System.Text.RegularExpressions;

string text = "Jeg er en programmerer.";
string pattern = "programmerer";
string replacement = "utvikler";

Regex regex = new Regex(pattern);
string newText = regex.Replace(text, replacement);
Console.WriteLine(newText);
```

Dette vil erstatte "programmerer" med "utvikler" i tekststrengen og printe ut "Jeg er en utvikler." Regex kan være mer komplekst og kraftig, så det kan være lurt å utforske dette konseptet videre for å utnytte det fullt ut.

## Dykk dypere:

Søk og erstatt av tekst er en funksjon som har vært tilgjengelig i ulike programmeringsspråk i mange år. Det er utformet for å effektivisere oppgaven med å manuelt endre tekst i store programmeringsprosjekter. Det finnes også alternative måter å gjøre søk og erstatning på, for eksempel ved hjelp av tredjepartsbiblioteker som tilbyr mer avanserte funksjoner.

For å implementere søk og erstatting i eget kode, er det viktig å ha god forståelse av strenger og Regex. Å lære om metall-reprasentasjoner av tekst og hvordan regler uttrykkes i Regex vil være nyttig når du utvikler mer komplekse søk og erstattningsfunksjoner.

## Se også:

- [Microsoft dokumentasjon for `string.Replace()`](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netcore-3.1)
- [Microsoft dokumentasjon for Regex](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)