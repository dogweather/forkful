---
title:                "Utvinne understrenger"
html_title:           "Python: Utvinne understrenger"
simple_title:         "Utvinne understrenger"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/extracting-substrings.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Extracting substrings er en prosess som innebærer å hente ut en del av en tekststreng basert på et visst kriterium. Dette er en vanlig oppgave for programmerere når de ønsker å arbeide med en mindre del av en større streng, for eksempel når de ønsker å hente ut et bestemt ord eller tall fra en tekst.

# Hvordan:
Det finnes flere måter å ekstrahere substrings på i Python, men en vanlig metode er å bruke slice-funksjonen. Denne funksjonen lar deg spesifisere start og sluttindekser for den delen av tekststrengen du ønsker å hente ut. Se eksempelet nedenfor for å få en bedre forståelse av prosessen.
```Python
tekst = "Dette er en tekststreng"
print(tekst[5:8]) # Output: er
```

# Dykk Dypere:
Ekstrahering av substrings har vært en viktig del av programmering siden de tidligste språkene ble utviklet. I dag er det fortsatt en viktig oppgave for mange programmerere, og det finnes flere alternativer til slice-funksjonen, for eksempel regex (regular expressions) som gir mer avansert funksjonalitet. Når det kommer til implementering, bruker Python en indekseringsmetode som starter med 0, noe som kan være forvirrende for nye brukere.

# Se også:
For mer informasjon om substring ekstraksjon i Python, sjekk ut Pythons offisielle dokumentasjon: https://docs.python.org/3/library/stdtypes.html#str.slice 
Du kan også lære mer om regular expressions og deres anvendelse i substring ekstraksjon her: https://www.regular-expressions.info/