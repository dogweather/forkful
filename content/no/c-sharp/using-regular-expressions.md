---
title:    "C#: Å bruke regulære uttrykk"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Hvorfor
Regular expressions er et kraftig verktøy for å søke og manipulere tekststrenger i programmering. Ved å bruke regex, kan du enkelt finne og manipulere data som passer et bestemt mønster. Dette kan spare deg for mye tid og arbeid i en rekke forskjellige situasjoner, som å validere brukerinput, søke i store tekstfiler, og mye mer.

## Hvordan
For å bruke regular expressions i C#, må du først importere System.Text.RegularExpressions biblioteket. Deretter kan du bruke Regex-klassen til å søke etter og manipulere tekststrenger.

La oss se på et enkelt eksempel der vi ønsker å finne og erstatte alle forekomster av ordet "hund" med "katt" i en tekststreng:

```C#
using System.Text.RegularExpressions;

string tekst = "Jeg liker hunder, men jeg er allergisk mot dem.";
string nyTekst = Regex.Replace(tekst, "hund", "katt");
Console.WriteLine(nyTekst);

// Output: Jeg liker katter, men jeg er allergisk mot dem.
```

Her er noen vanlige metoder og egenskaper på Regex-klassen som kan være nyttige i ditt arbeid med regular expressions:

- `IsMatch()` - sjekker om en tekststreng samsvarer med et gitt regex-mønster
- `Match()` - returnerer en Match-objekt som inneholder informasjon om det første samsvarer mellom en tekststreng og et regex-mønster
- `Matches()` - returnerer en MatchCollection-objekt som inneholder alle samsvarer mellom en tekststreng og et regex-mønster
- `Split()` - deler en tekststreng basert på et regex-mønster
- `Replace()` - erstatter samsvarer i en tekststreng med et gitt mønster eller en gitt tekst

For å lære mer om hvordan du bruker regex i C#, kan du sjekke ut [dokumentasjonen til Regex-klassen](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=netcore-3.1) og [regulære uttrykk på w3schools](https://www.w3schools.com/jsref/jsref_obj_regexp.asp).

## Dypdykk
Det finnes mange forskjellige regex-mønstre og teknikker du kan bruke for å matche og manipulere tekst. Her er noen få tips som kan hjelpe deg å bli mer komfortabel med regular expressions:

- Bruk [regex tester](https://regex101.com/) for å eksperimentere og teste dine regex-mønstre.
- Uttrykket `.` betyr "any character", `*` betyr "zero eller flere forekomster", og `+` betyr "én eller flere forekomster". Disse er grunnleggende operatorer som kan være nyttige å lære seg.
- Du kan bruke `\b` for å matche ordgrenser, `\d` for å matche tall, og `\w` for å matche bokstaver, tall og understrekere.
- Ved hjelp av parenteser kan du gruppere deler av et regex-mønster for senere å bruke dette i erstatningen. For eksempel kan du bruke `(\w+) (\w+)` for å matche to ord og bruke `$2, $1` i erstatningen for å bytte plass på dem.
- Regex-mønstre er vanligvis case-sensitive, men du kan bruke flaggene `RegexOptions.IgnoreCase` eller `RegexOptions.IgnorePatternWhitespace` for å gjøre søket case-insensitive eller ignorere mellomrom og kommentarer i mønsteret.

Bare husk at regular expressions kan virke forvirrende og komplekst i begynnelsen, men etter hvert som du blir mer kjent med det, vil du oppdage at det er et kraftig verktøy som kan hjelpe deg å løse utfordringer i dine programmer.

## Se også
- [Regex Tutorial på w3schools](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [Regex Cheat Sheet på DevHints](https://devhints.io/regex)
- [Regex Tutorial på ProgrammeringsWiki](https://programmeringswiki.no/wiki/Reg