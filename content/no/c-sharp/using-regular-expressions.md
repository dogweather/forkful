---
title:    "C#: Å bruke regulære uttrykk"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er en programmerer, har du sannsynligvis hørt om "regular expressions" før. Men hva er det egentlig, og hvorfor bør du bruke det i dine programmeringsprosjekter?

Regular expressions, også kjent som "regex", er en måte å søke og manipulere tekststrenger på. Dette kan være nyttig i mange tilfeller, som for eksempel når du vil finne eller erstatte bestemte mønstre i en tekst eller søke gjennom store mengder data på en effektiv måte.

## Slik gjør du det

For å bruke regex i C# trenger du bare å inkludere "System.Text.RegularExpressions" namespace i prosjektet ditt. Deretter kan du enkelt lage et regex-objekt og bruke forskjellige metoder som "Match" og "Replace" for å manipulere tekststrenger.

Her er et eksempel på hvordan du kan bruke regex for å finne og erstatte spesifikke ord i en tekst:

```C#
using System;
using System.Text.RegularExpressions;

string tekst = "Hei, jeg heter John og jeg elsker å kode. Koding er gøy!";

Regex regex = new Regex("kode");
string nyTekst = regex.Replace(tekst, "programmering");

Console.WriteLine(nyTekst);

// Output:
// Hei, jeg heter John og jeg elsker å programmering. Programmering er gøy!
```

Som du kan se, bruker vi først "Regex" klassen til å lage et regex-objekt med søkeordet "kode". Deretter bruker vi "Replace" metoden til å erstatte alle forekomster av "kode" med "programmering" i teksten vår. Det er viktig å merke seg at regex er case sensitive, så "kode" og "Kode" vil ikke bli erstattet.

## Dykk dypere

Det er mange forskjellige måter å bruke regex på i C#, og det er umulig å dekke alt i en kort bloggpost. Men her er noen tips som kan hjelpe deg på veien til å mestre regular expressions:

- Regex kan også brukes til å validere input fra brukere, som for eksempel e-postadresser eller telefonnumre.
- Det er forskjellige "spesielle tegn" som kan brukes i regex for å søke etter bestemte mønstre, som for eksempel "w" for å finne bokstaver og "d" for å finne tall.
- Hvis du trenger å finne og erstatte mønstre i en tekst som inneholder spesielle tegn, kan du bruke escape-tegn "\" for å fortelle regex at disse tegnene skal behandles som vanlig tekst.

## Se også

- [Offisiell dokumentasjon for regex i C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Regex Tutorial på W3Schools](https://www.w3schools.com/python/python_regex.asp)
- [10 Essential Tips for Regular Expressions in C#](https://www.codeproject.com/Articles/9099/The-30-Minute-Regex-Tutorial) (engelsk)