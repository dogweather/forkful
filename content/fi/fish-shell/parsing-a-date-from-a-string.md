---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Bash: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Fish Shell: Päivämäärän parsiminen merkkijonosta

## Mikä & Miksi?
Päivämäärän parsiminen merkkijonosta tarkoittaa päivämäärän erottamista tekstistä ja sen käsittelyä ohjelmoimista varten. Ohjelmoijat tekevät näin, jotta he voivat käsitellä päivämäärää järjestelmässään haluamallaan tavalla.

## Näin teet:
Alla on esimerkkejä siitä, kuinka päivämäärä voidaan parsia Fish Shellissä. 

```Fish Shell
set päivä "20248-09-15"
set -l päivä_osa (string split -r -m1 "-" $päivä)

echo "Vuosi: "$päivä_osa[1]
echo "Kuukausi: "$päivä_osa[2]
echo "Päivä: "$päivä_osa[3]
```
Tämä koodi tuottaa seuraavan tulosteen: 

```Fish Shell
Vuosi: 2024
Kuukausi: 09
Päivä: 15
```

## Syvemmälle:
### Historiallinen Konteksti
Päivämäärän parsiminen merkkijonosta on ollut tärkeä taito ohjelmoinnissa jo useita vuosia. Se helpottaa ohjelmiston kehittämistä, sillä päivämäärä voidaan muuntaa halutulle formaatille tai sitä voidaan käyttää laskelmissa ja vertailuissa.
### Vaihtoehtoja
Fish Shellin lisäksi on olemassa monia muita ohjelmointikieliä, jotka tarjoavat päivämäärien parsimisen, kuten Python, JavaScript ja PHP. Vaikka nämä kielet eroavat Fish Shellistä, niillä kaikilla on samanlaiset perusmekanismit päivämäärien parsimiseen. 
### Toteutus
Tärkeintä päivämäärän parsimisessa merkkijonosta on merkkijonon oikea jakaminen. Tässä esimerkissä käytämme Fish Shellin `string split` -toimintoa, joka jakaa merkkijonon "-" -merkin kohdalta. Jakamisen jälkeen jokainen osa (vuosi, kuukausi, päivä) tallennetaan erilliseen muuttujaan.

## Katso myös:
Lisätietoa päivämäärien parsimisesta ja Fish Shellin koodauksesta:
1. Fish Shellin kotisivu: [https://fishshell.com/](https://fishshell.com/)
2. StackOverflow, Fish Shell -aiheiset keskustelut: [https://stackoverflow.com/questions/tagged/fish](https://stackoverflow.com/questions/tagged/fish)
3. GitHub, Fish Shell -projekti: [https://github.com/fish-shell/fish-shell](https://github.com/fish-shell/fish-shell)