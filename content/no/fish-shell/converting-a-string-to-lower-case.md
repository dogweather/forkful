---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å konvertere en streng til små bokstaver innebærer å endre alle store bokstaver i teksten til deres tilsvarende små bokstaver. Programmerere gjør dette for å oppnå konsistent datainnsamling og -behandling, spesielt i tekstbehandling og søk algoritmer.

## Hvordan:
Du kan enkelt konvertere en streng til små bokstaver i Fish shell ved hjelp av følgende kode.

```
set streng 'Hei Verden'
echo $streng | tr '[:upper:]' '[:lower:]'
```

Når du kjører denne koden, vil du se følgende resultat:
```
hei verden
```

## Dypdykk:
Historisk sett har konvertering av tekst til små bokstaver vært en viktig funksjon i mange programmeringsspråk for å forenkle strengsammenligning og indeksering. I Fish shell bruker vi 'tr' kommandoen for å oppnå dette. Men du kan også bruke 'awk' eller 'perl' for samme formål hvis du vil.

```
# Med awk
echo $streng | awk '{print tolower($0)}'

# Med perl
echo $streng | perl -ne 'print lc'
```

Hver av disse mulighetene har egne fordeler. For eksempel lar 'tr' deg enkelt bytte mellom store og små bokstaver. Imens 'awk' og 'perl' gir mer kontroll over dataanalyse og -manipulering.

## Se Også: 
For mer informasjon om hvordan du bruker disse kommandoene, kan du ta en titt på de følgende kildene:

- 'tr' kommandobruk: [GNU Coreutils - tr](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html#tr-invocation)
- 'awk' kommandobruk: [GNU Awk User’s Guide](https://www.gnu.org/software/gawk/manual/gawk.html)
- 'perl' kommando bruk: [Perl Command-Line Options](https://perldoc.perl.org/perlrun)