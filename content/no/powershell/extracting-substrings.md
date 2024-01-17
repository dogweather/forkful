---
title:                "Uthenting av undertekster"
html_title:           "PowerShell: Uthenting av undertekster"
simple_title:         "Uthenting av undertekster"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å trekke ut delstrenger, også kjent som substringer, er en vanlig oppgave for programmerere når de arbeider med tekstbaserte data. Dette innebærer å hente ut en del av en lengre tekststreng basert på visse kriterier, for eksempel å finne et bestemt ord eller tall. Dette er nyttig for å behandle og analysere store datasett, og hjelper programmerere med å finne spesifikke informasjoner raskt.

## Slik gjør du det:

Du kan bruke PowerShell til å trekke ut substringer fra en tekststreng ved hjelp av `Substring`-metoden. La oss si at vi har en tekststreng som heter `$Tekst = "Hei verden!"` og vi vil trekke ut ordet "verden". Da kan vi bruke følgende kommandoer:

```PowerShell
$Tekst.Substring(4, 6)
```

Her sier vi at vi vil starte på indeks 4 (som tilsvarer bokstaven "v") og ta seks tegn fra dette punktet. Resultatet vil være `verden`, som er ordet vi ønsker å trekke ut.

Du kan også bruke `Substring`-metoden til å finne og trekke ut tall fra en tekststreng. For eksempel, hvis vi har en tekststreng som heter `$Tekst = "I år er det 2021"`, og vi ønsker å trekke ut tallet 2021, kan vi bruke følgende kommandoer:

```PowerShell
$Tekst.Substring(10, 4)
```

Her sier vi at vi vil starte på indeks 10 (som tilsvarer tallet 2) og ta fire tegn fra dette punktet. Resultatet vil være `2021`.

## Dykk dypere:

Extracting substrings har vært en vanlig oppgave for programmerere siden de første tekstbaserte dataprogrammene ble utviklet. Før i tiden ble det ofte gjort ved hjelp av "manipulering" av tekststrenger, som innebar å bruke kompliserte formelsett for å hente ut deler av en tekststreng. Med PowerShell kan du enkelt bruke `Substring`-metoden for å oppnå det samme resultatet.

Det finnes også alternative metoder for å trekke ut substringer, som for eksempel `Regex` (regulære uttrykk) og `Split`-metoden. Disse kan være nyttige i andre situasjoner og det kan være lurt å undersøke dem for å finne den beste løsningen for ditt spesifikke behov.

Når det kommer til implementeringsdetaljer, er det viktig å merke seg at indeksering i PowerShell starter på 0. Det betyr at den første bokstaven i en tekststreng er på indeks 0, og ikke 1 som man vanligvis ville trodd. Dette kan være viktig å huske på når du skal hente ut deler av en tekststreng ved hjelp av `Substring`-metoden.

## Se også:

Hvis du ønsker å lære mer om hvordan du kan arbeide med tekststrenger i PowerShell, kan du sjekke ut følgende kilder:

- [PowerShell dokumentasjon](https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-7.1)
- [PowerShell for nybegynnere](https://www.udemy.com/course/powershell-for-beginners/)
- [Offisiell PowerShell-blogg](https://devblogs.microsoft.com/powershell/)