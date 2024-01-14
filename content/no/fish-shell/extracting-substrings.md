---
title:                "Fish Shell: Uttrekk av delstrenger"
simple_title:         "Uttrekk av delstrenger"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen ha behov for å trekke ut substringer? Vel, noen ganger kan det være nødvendig å manipulere en tekststreng for å få den til å passe til spesifikke formater eller begrensninger. Ved å kunne ekstrahere deler av en streng kan man lettere håndtere og manipulere data.

## Slik gjør du det

For å ekstrahere en del av en streng i Fish Shell, kan du bruke kommandoen `string --substring`. Den tar to argumenter: startindeks og lengden på ønsket substring. La oss se på et eksempel for å se hvordan dette fungerer:

```Fish Shell
set streng “Hei, dette er en tekststreng”
echo $streng[8..15]
```

I dette eksemplet trekker vi ut delen av strengen som starter på indeks 8 (den 9. bokstaven) og har en lengde på 8 tegn. Outputen av dette vil være “er en tek”.

Det er også mulig å bruke negative tall for å starte fra slutten av strengen. For eksempel vil `$streng[-8..-1]` gi oss delen av strengen som starter 8 tegn fra slutten og går til siste tegn i strengen.

## Dypdykk

Det finnes også flere måter å ekstrahere substringer på i Fish Shell. For eksempel kan vi bruke `string match` kommandoen for å finne en del av en streng basert på et mønster. Dette kan være nyttig hvis man ikke vet nøyaktig hvor substringen starter eller slutter.

En annen viktig ting å huske på er at når man ekstraherer en del av en streng i Fish Shell, vil det resulterende utvalget være en liste av tegn. Hvis man ønsker å konvertere denne listen til en enkelt streng, kan man bruke `string join` kommandoen.

## Se også

- [Fish Shell dokumentasjon om substringer](https://fishshell.com/docs/current/cmds/set.html#substrings)
- [Tips og triks for å jobbe med strenger i Fish Shell](https://dev.to/scrool/working-with-strings-in-fish-shell-1ag0)