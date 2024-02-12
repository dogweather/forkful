---
title:                "Päivämäärän jäsennys merkkijonosta"
aliases:
- /fi/fish-shell/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:16.479293-07:00
model:                 gpt-4-0125-preview
simple_title:         "Päivämäärän jäsennys merkkijonosta"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Päivämäärän jäsentäminen merkkijonosta tarkoittaa päivämäärätiedon erottelua merkkijonoista ja sen muuntamista rakenteelliseen muotoon, jonka ohjelmointiympäristöt voivat tunnistaa ja käsitellä. Ohjelmoijat tekevät tämän mahdollistaakseen toimenpiteitä, kuten päivämäärien vertailun, aritmetiikan, muotoilun ja lokalisoinnin, jotka ovat olennaisia aikataulujen, aikaleimojen ja historiallisen datan tehokkaalle käsittelylle ohjelmistoissa.

## Kuinka:
Fish Shellissä ei ole sisäänrakennettuja komentoja erityisesti merkkijonoista päivämäärien jäsentämiseksi suunniteltuina. Sen sijaan tukeudutaan ulkoisiin työkaluihin, kuten `date` (saatavilla Linuxissa ja macOS:ssa) tai hyödynnetään suosittuja kolmannen osapuolen työkaluja, kuten `GNU date` monimutkaisempiin jäsentämisiin. Näin se tehdään:

**Käyttäen `date` Fishissä:**

Jäsentääksesi päivämäärämerkkijonon muodossa "VVVV-KK-PP", voit käyttää `date`-komentoa `-d` (tai `--date` GNU datelle) valitsimen kanssa seurattuna merkkijonosta. `+`-valitsinta käytetään tulosteen muotoiluun.

```fish
set date_str "2023-04-01"
date -d $date_str +"%A, %d %B %Y"
# Tuloste: Lauantai, 01 Huhtikuu 2023
```

macOS:lle (joka vaatii erilaisen muodon `-j` ja `-f` lipuille):

```fish
set date_str "2023-04-01"
date -j -f "%Y-%m-%d" $date_str +"%A, %d %B %Y"
# Tuloste: Lauantai, 01 Huhtikuu 2023
```

**Käyttäen GNU `date` monimutkaiseen jäsentämiseen:** 

GNU `date` on joustavampi merkkijonojen muotojen kanssa. Se voi automaattisesti tunnistaa monia yleisiä päivämäärämerkkijonojen muotoja ilman, että syöttömuotoa tarvitsee nimenomaisesti määrittää:

```fish
set complex_date_str "Huhtikuu 1, 2023 14:00"
date -d "$complex_date_str" '+%Y-%m-%d %H:%M:%S'
# Tuloste: 2023-04-01 14:00:00
```

Kuitenkin, kun työskennellään päivämäärämerkkijonojen kanssa, joita ei voida automaattisesti tunnistaa tai kun tarkan kontrollin syöttömuodosta tarvitaan, GNU `date`n syöttömuodon määrittely ei ole suoraan tuettua. Tällaisissa tapauksissa harkitse merkkijonon esikäsittelyä tai toisen työkalun käyttöä, joka on suunniteltu monimutkaisempiin päivämääräjäsentämisen rutiineihin.
