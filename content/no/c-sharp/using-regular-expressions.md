---
title:                "Bruk av regulære uttrykk"
html_title:           "C#: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen engasjere seg i å bruke regulære uttrykk? Vel, regulære uttrykk er et utrolig kraftig verktøy som kan hjelpe deg med å finne og manipulere tekst i enkle og komplekse måter. Dette er spesielt nyttig for utviklere som jobber med tekstbehandling, dataanalyse eller testing.

## Slik gjør du det

La oss si at du ønsker å finne alle e-postadresser i en tekstfil. Dette kan være en utfordrende oppgave hvis du må lete gjennom hundrevis av linjer med tekst. Men med regulære uttrykk kan du enkelt finne alle forekomster av e-postadresser ved hjelp av et mønster.

La oss se på et eksempel i C#:

```C#
string text = "Dette er en tekst med flere e-postadresser som [email protected] og [email protected]";
string pattern = @"\w+([-+.']\w+)*@\w+([-.]\w+)*\.\w+([-.]\w+)*";
Regex regex = new Regex(pattern);
MatchCollection matches = regex.Matches(text);

foreach (Match match in matches)
{
    Console.WriteLine(match.Value);
}
```

Dette vil gi følgende output:

```
[email protected]
[email protected]
```

La oss bryte ned koden litt:

- Først oppretter vi en tekststreng som inneholder e-postadressene vi vil finne.

- Deretter definerer vi et mønster som beskriver en vanlig e-postadresse. Dette mønsteret bruker såkalte regex-koder, som representerer forskjellige tegn og mønstre.

- Vi oppretter et Regex-objekt med mønsteret vårt.

- Deretter bruker vi Matches-metoden til å finne alle forekomster av mønsteret i teksten vår.

- Til slutt går vi gjennom alle kampene og skriver dem ut.

## Dykk dypere

Dette var bare et enkelt eksempel på hvordan du kan bruke regulære uttrykk i C#. Det er mange forskjellige regex-koder og teknikker du kan lære for å gjøre søkene og manipuleringen av tekst enda mer nøyaktig og effektiv.

Et annet nyttig aspekt ved regulære uttrykk er at de kan brukes i flere programmeringsspråk og tekstredigeringsverktøy. Så ved å lære regex i C#, vil du også kunne bruke det i f.eks. Javascript, Java eller Python.

## Se også

- [Fullstendig veiledning til regulære uttrykk i C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [En online regex-tester](https://regex101.com/)
- [Regex Crash Course video](https://www.youtube.com/watch?v=rhzKDrUiJVk)