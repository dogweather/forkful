---
title:    "C#: Slette tegn som matcher et mønster"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Å slette tegn som matcher et mønster kan være nyttig i situasjoner der man ønsker å fjerne uønskede tegn fra en streng, for eksempel i et tekstbehandlingsprogram eller en database. Dette kan også være en viktig del av å validere brukerinput i et program.

## Hvordan Gjøre Det

For å slette tegn som matcher et mønster, kan man bruke metoden `Regex.Replace()`. Dette er en innebygd metode i C# som tar inn tre parametre: en streng, et mønster og det nye tegnet man ønsker å erstatte det gamle med. La oss se på et eksempel:

```C#
string input = "d0g c@t f1sh";
string pattern = "[^a-zA-Z0-9]"; // Dette mønsteret matcher alle tegn som ikke er bokstaver eller tall
string output = Regex.Replace(input, pattern, "");
Console.WriteLine(output); // Output vil være "d0g c@t f1sh"
```

I dette eksempelet bruker vi metoden til å slette alle tegn som ikke er bokstaver eller tall i den opprinnelige strengen `input`. Resultatet blir lagret i strengen `output` og deretter skrevet ut til konsollen.

## Dypdykk

Vi kan også bruke `Regex.Replace()` til å erstatte tegn med andre tegn eller strenger. For eksempel kan vi slette alle mellomrom i en streng ved å bruke følgende mønster: `\s+`. Dette vil matche alle mellomrom og erstatte dem med ingenting, noe som vil resultere i en streng uten mellomrom.

Det er også verdt å nevne at man kan bruke regulære uttrykk i stedet for en streng som mønster. Dette gir større fleksibilitet og mulighet for mer avanserte mønstre.

## Se Også

- [Microsofts offisielle dokumentasjon for Regex.Replace()](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace?view=netcore-3.1)
- [En guide til regulære uttrykk i C#](https://www.regular-expressions.info/dotnet.html)
- [En oversikt over ASCII-tegn](https://www.ascii-code.com/)