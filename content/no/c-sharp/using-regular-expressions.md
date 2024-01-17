---
title:                "Å bruke regulære uttrykk"
html_title:           "C#: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Vanligvis innebærer programmering å jobbe med tekst. Men når vi har store datamengder, kan det være vanskelig å finne eller manipulere nøyaktig det vi trenger. Her kommer regulære uttrykk (regular expressions) inn i bildet. Dette er en måte å søke, manipulere og erstattet tekst basert på et gitt mønster. Programmere bruker dette for å effektivt håndtere store datamengder og automatisere repetitive oppgaver.

## Hvordan:
Bruk av regulære uttrykk er enkelt og kan spare deg for mye tid og frustrasjon i det lange løp. Her er et eksempel på hvordan du kan finne alle e-postadresser i en tekst ved hjelp av regulære uttrykk i C#:

```C#
string text = "Hei! Min e-post er eksempel@eksempel.com. Vennligst kontakt meg hvis du har spørsmål.";
Regex regex = new Regex(@"\w+@\w+\.\w+"); // Regex objekt, her brukes \w som matcher alle bokstaver og tall
MatchCollection matches = regex.Matches(text); // Alle forekomster av e-postadresser i teksten lagres i en MatchCollection
foreach (Match match in matches)
{
    Console.WriteLine(match.Value); // Printer ut hver match - her vil den skrive ut "eksempel@eksempel.
}

// Output:
// eksempel@eksempel.
```

## Dypdykk:
Regulære uttrykk ble introdusert på 1950-tallet av matematikerne Stephen Kleene og Ken Thompson. De benyttes i en rekke programmeringsspråk, og gir en robust og effektiv måte å håndtere tekstbehandling på. Alternativer til bruk av regulære uttrykk inkluderer manuell tekstbehandling og bruk av string-metoder i C#. Implementasjonen av regulære uttrykk i C# er basert på syntaksen brukt av Perl.

## Se også:
- [Microsoft sin guide til regulære uttrykk i C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Regulære uttrykk på Wikipedia](https://en.wikipedia.org/wiki/Regular_expression)
- [RegexTester - et nyttig verktøy for å teste regulære uttrykk](https://regexr.com/)